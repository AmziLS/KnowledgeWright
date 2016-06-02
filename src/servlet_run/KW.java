import amzi.ls.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.util.*;

// KW.java -- KnowledgeWright Servlet Interface
// Copyright (c)2002 Amzi! inc. All Rights Reserved.

// To do:
// Implement KWI save session command and call it when no more to replace save_session in init

public class KW extends HttpServlet
// SingleThreadModel is required for production environments with multiple user sessions
   implements SingleThreadModel
{
   ServletContext context;

   // Initialize global variables
   public void init(ServletConfig config) throws ServletException
   {
      super.init(config);
      context = config.getServletContext();

//     throw new UnavailableException (this, "message");
   }

   // Destroy global resources
   public void destroy()
   {
      super.destroy();
   }

   // Process the HTTP Get request
   public synchronized void doGet (HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      // Get the user's session with logic server and KW id (must do this first!)
      HttpSession session = request.getSession(true);

      // Must be called before getting values
      String charset = (String)session.getAttribute("kw.charset");
      if (charset != null && charset.length() > 0)
         try
         {
            request.setCharacterEncoding(charset);
         }
         catch (UnsupportedEncodingException ex)
         {
            throw new ServletException("doGet failed because charset encoding "
               + charset + " is not supported");
         }

      // Use the session id for KW
      String id = "kw_" + session.getId();

      // If a kb_file is specified, we need to restart from the top
      if (request.getParameter("kb_file") != null)
      {
         LogicServer ls = (LogicServer)session.getAttribute("kw.logicserver");
         if (ls != null)
         {
            // Remove all the attributes
            session.removeAttribute("kw.logicserver");
            session.removeAttribute("kw.charset");
            session.removeAttribute("kw.kb_file");

            // And close the logic server
            try
            { ls.Close(); }
            catch (LSException ex)
            { };
         }

         // Then start from the top
         startKB(request, response, session, id, false);
      }
      else
      {
         // See if we're already rolling
         LogicServer ls = (LogicServer)session.getAttribute("kw.logicserver");
         if (ls == null)
         {
            // Check if the user pressed the back button in the browser
            if (request.getParameter("kb_file") == null && session.getAttribute("kw.kb_file") != null)
               startKB(request, response, session, id, true);
            // Otherwise this is a clean start
            else
               startKB(request, response, session, id, false);
         }
         else
         {
            // Assert the new facts
            assertFacts(request, session, ls, id);

            // Run the knowledgebase
            runKB(request, response, session, ls, id);
         }
      }
   }

   private void assertFacts(HttpServletRequest request, HttpSession session, LogicServer ls, String id) throws ServletException
   {
      try
      {
         // Backup in case the user went back to a prior form
         Enumeration factNames = request.getParameterNames();
         while (factNames.hasMoreElements())
         {
            String name = (String)factNames.nextElement();

            // Don't backup to the submit button
            if (name.compareToIgnoreCase("submit") != 0)
               if (ls.ExecStr("kwi('" + id + "', backup(fact('" + name + "', _)), _)") == 0)
                  throw new ServletException("kwi / backup to fact " + name + " failed");
         }

         // Assert the new data
         factNames = request.getParameterNames();
         while (factNames.hasMoreElements())
         {
            String name = (String)factNames.nextElement();

            // Ignore the submit button
            if (name.compareToIgnoreCase("submit") == 0) continue;

            String values[] = request.getParameterValues(name);
            if (values != null)
            {
               if (values.length == 1)
               {
                  if (ls.ExecStr("kwi('" + id + "', assert(fact('" + name + "', $" + values[0] + "$)), _)") == 0)
                     throw new ServletException("kwi / assert failed for fact " + name + " value " + values[0]);
               }
               else
               {
                  String factList = "";
                  for (int i = 0 ; i < values.length ; i++)
                  {
                     factList = factList + "$" + values[i] + "$,";
                  }
                  String s = "kwi('" + id + "', assert(fact('" + name + "', [" + factList.substring(0,factList.length()-1) + "])), _)";
                  if (ls.ExecStr(s) == 0)
                     throw new ServletException("kwi /assert failed for fact " + name + " value " + factList);
               }
            }
         }
      }
      catch (LSException ex)
      {
         throw new ServletException(formatKBError(ex, ls, "'" + id + "'"));
      }
   }

   private void startKB(HttpServletRequest request, HttpServletResponse response, HttpSession session, String id, boolean backup) throws ServletException, IOException
   {
      String s, path, kbname, jig, zid = "0";
      long term;
      LogicServer ls = null;
      File sessionDir;

      try
      {
         // Initialize a new Logic Server
         ls = new LogicServer();
         ls.Init("");

         // Load OSUtils
         ls.AddLSX("aosutils", 0);

         // Load ODBC (optional, you can take this out if you don't use it)
         // Alternatively you could call ls.InitLSX() which will load any
         // LSXs specified in amzi.cfg
         String os = System.getProperty("os.name");
         if ( os != null && os.startsWith("Windows"))
            ls.AddLSX("aodbc", 0);

         // Determine where we are
         path = request.getParameter("directory");
         if (path == null)
         {
            String servletPath = context.getRealPath(request.getServletPath());
            path = servletPath.substring(0, servletPath.lastIndexOf(System.getProperty("file.separator")));
         }
         path = path + System.getProperty("file.separator");

         // Clean up old session files
         if (request.getParameter("temp_directory") != null)
            sessionDir = new File(request.getParameter("temp_directory"));
         else
            sessionDir = new File(path);
         ExtFilenameFilter filter = new ExtFilenameFilter("kws");
         File sessionFiles[] = sessionDir.listFiles(filter);
         if (sessionFiles != null)
         {
            for (int i = 0 ; i < sessionFiles.length ; i++)
               // Delete it if its more than a day old
               if (sessionFiles[i].lastModified() < (System.currentTimeMillis() - 24*3600))
                  sessionFiles[i].delete();
         }

         // Determine the Jig from the KB file
         if (backup)
            kbname = (String)session.getAttribute("kw.kb_file");
         else
            kbname = request.getParameter("kb_file");
         if (kbname == null) throw new ServletException("No kb_file specified in URL or saved in session");
         BufferedReader br = new BufferedReader(new FileReader(new File(path + kbname)));
         String line = br.readLine();
         if (line == null)
            throw new ServletException("Unable to find jig name in kb file");
         jig = line.substring(line.indexOf("(")+1, line.indexOf(","));

         // Load the Jig
         ls.Load(path + jig + ".xpl");

         // Save the logic server
         session.setAttribute("kw.logicserver", ls);

         // Initialize the runtime with directory and logfile
         // Prior to starting a new KWI session the session id must be kw_init.
         zid = "kw_init";

         // The directory is where the kb file is located
         // The session_directory is where a temporary file is created to maintain the state
         //   of the reasoning process
         // The log_file is a log of the reasoning process and is created in the directory
         // The message level is the level of detail in the log_file
         s = "kwi(" + zid + ", initialize([directory = $" + Utils.doubleSlashes(path) + "$, ";
         if (request.getParameter("temp_directory") == null)
            s = s + "session_directory = $" + Utils.doubleSlashes(path) + "$, ";
         else
            s = s + "session_directory = $" + Utils.doubleSlashes(request.getParameter("temp_directory")
               + System.getProperty("file.separator")) + "$,";
         if (request.getParameter("log_file") != null)
            s = s + "log_file = $" + request.getParameter("log_file") + "$, message_level = high, ";
         else
            s = s + "message_level = none, ";
         s = s + "session_save = true]), _INFO)";
         term = ls.ExecStr(s);
         if (term == 0)
            throw new ServletException("kwi / initialize failed");

         // Get the name, version and build (optional)
         Properties sysInfo = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 5000);
         String engineName = sysInfo.getProperty("system");
         String version = sysInfo.getProperty("version");

         // Open the knowledgebase, and save it in the session
         session.setAttribute("kw.kb_file", kbname);     // Save if for browser back button
         s = "kwi(" + zid + ", open($" + kbname + "$), _)";
         term = ls.ExecStr(s);
         if (term == 0)
            throw new ServletException("kwi / open " + kbname + " failed");

      }
      catch (LSException ex)
      {
         if (ls != null)
            throw new ServletException("startKB / " + formatKBError(ex, ls, "'" + zid + "'"), ex);
      }

      try
      {
         // Backup the reasoning to the specified facts then assert the new values
         if (backup)
            assertFacts(request, session, ls, id);
         // Otherwise start a new session
         else
         {
            // Use our unique session ID
            if (ls.ExecStr("kwi('" + id + "', new_session, _)") == 0 )
               throw new ServletException("kwi / new_session failed");
         }
      }
      catch (LSException ex)
      {
         if (ls != null)
            throw new ServletException("startKB / " + formatKBError(ex, ls, "'" + id + "'"), ex);
      }

      runKB(request, response, session, ls, id);
   }

   private void runKB(HttpServletRequest request, HttpServletResponse response, HttpSession session, LogicServer ls, String id) throws ServletException, IOException
   {
      long solveTerm, actionTerm, responseTerm, charsetTerm;
      boolean more = true;
      String charset = "", responseFunctor, responseType;
      PrintWriter out = null;

      try
      {
         // set content type (and other response header fields) first
         String ctype = "text/html";
         if ((charsetTerm = ls.ExecStr("kwi('" + id + "', get_parm(knowledgebase, main, charset), _CHARSET)")) != 0)
         {
            charset = ls.GetStrArg(charsetTerm, 3);
            if (charset.length() > 0) ctype = ctype + "; charset="+charset;

            // Save it for assert
            session.setAttribute("kw.charset", charset);
         }
         response.setContentType(ctype);
         response.setHeader("Cache-Control", "no-cache");

         // Now open our output, must be done after setContentType!
         out = response.getWriter();

         // Call solve to get the next set of actions
         if ((solveTerm = ls.ExecStr("kwi('" + id + "', solve, _MORE)")) != 0)
         {
            // Check if we're done
            if (!ls.GetStrArg(solveTerm, 3).equals("more"))
               more = false;

            // Process all the actions. Actions are either questions to
            // ask the user or information to tell the user.
            do
            {
               // Get the action
               if ((actionTerm = ls.ExecStr("kwi('" + id + "', get_action, _ACTION)")) == 0)
                     throw new ServletException("kwi / get_action failed");

               // The third parameter in the kwi call contains the type and
               // parameters of the action to be performed
               // Use the Amzi! Logic Server to retrieve the action
               responseTerm = ls.GetArg(actionTerm, 3);
               responseFunctor = ls.GetFunctor(responseTerm);
               responseType = ls.GetStrArg(responseTerm, 1);

               // Display the HTML question page (we ignore this...used by the web interface)
               if (responseFunctor.equals("ask") && responseType.equals("html"))
               {
                  // Return the html formatted questions
                  Properties question = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 3), 100000);

                  // Output them
                  out.print(question.getProperty("html"));
               }

               // Display an answer
               if (responseFunctor.equals("tell") && responseType.equals("user"))
               {
                  // Get all the slots in the answer into a Properties object
                  Properties answer = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 2), 100000);

                  // Get the name of the answer
                  String answerName = answer.getProperty("goal");

                  // Output the solution
                  String solution = answer.getProperty("text");
                  if (solution != null)
                     out.print(solution);
                  else
                     throw new ServletException("No text for goal: " + answerName);
               }
            }
            while (!responseFunctor.equals("none"));

         } // if solveTerm

         // If we're done clean up everything
         // Commented out to allow backing up and restarting
         if (!more)
         {
//            if (ls.ExecStr("kwi('" + id + "', close, _)") == 0 )
//               throw new ServletException("kwi / close failed");
//            session.invalidate();
//            ls.Close();
//            session.removeAttribute("kw.logicserver");
         }
      }
      catch (LSException ex)
      {
         throw new ServletException("runKB / " + formatKBError(ex, ls, "'" + id + "'"));
      }

      if (out != null) out.close();
   }

   // Display errors from the KWI or the underlying Amzi! Logic Server
   private String formatKBError(LSException e, LogicServer ls, String id)
   {
      String s, msg;
      long term, errorList;

      try
      {
         // KBI errors return a detailed list of attributes
         if (e.GetMsg().indexOf("kwi_error(") >= 0)
         {
            s = "kwi(" + id + ", get_error, _ERROR)";
            term = ls.ExecStr(s);

            // If we can't get error information, don't die
            if (term == 0)
            {
               msg = "Unable to call kwi / get_error for: " + e.GetMsg();
               return msg;
            }

            // Get all the attributes of the error into a Properties object
            Properties info = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 10000);
            Enumeration propNames = info.propertyNames();

            // Build a nicely formatted message
            msg = "";
            while (propNames.hasMoreElements())
            {
               String name = (String)propNames.nextElement();
               if (name.equals("callstack")) continue;
               if (name.equals("rc"))
                  msg = msg + "Error #" + info.getProperty(name) + "\n";
               else if (name.equals("type"))
                  msg = msg + "Type: " + info.getProperty(name) + "\n";
               else if (name.equals("message"))
                  msg = msg + info.getProperty(name) + "\n";
               else if (name.equals("error"))
                  msg = msg + info.getProperty(name) + "\n";
               else
                  msg = msg + name + ": " + info.getProperty(name) + "\n";
            }
            return msg;
         }
         // Consult errors (outside the KWI)
         else if (e.GetType() == LSException.READ /*read*/)
         {
            String lineno = new Integer(e.GetLineno()).toString();
            msg = e.GetMsg() + "\n in file " + e.GetReadFileName() +
               "\n at line " + lineno + "\n" + e.GetReadBuffer();
            return msg;
          }

         // Unrecognized Logic Server error
         else
            return e.GetMsg();
      }
      catch (LSException e2)
      {
         return "Error catching error";
      }
   }

}

