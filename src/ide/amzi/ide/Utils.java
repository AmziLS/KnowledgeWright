
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:
package amzi.ide;

import amzi.ls.*;
import java.awt.*;
import java.io.*;
import java.util.*;
import com.borland.jb.util.*;

public class Utils
{
   public static void displayURL(String browser, String url) throws IOException, InterruptedException
   {
      // Used to identify the windows platform.
      String WIN_ID = "Windows";
      // The default system browser under windows.
      String WIN_PATH = "rundll32";
      // The flag to display a url.
      String WIN_FLAG = "url.dll,FileProtocolHandler";
      // The default browser under unix.
      String UNIX_PATH = "netscape";
      // The flag to display a url.
      String UNIX_FLAG = "-remote openURL";

      boolean windows;
      String cmd = null;

      // If a browser is specified in preferences, use it
      if (browser != null && browser.length() > 0)
      {
         WIN_PATH = browser;
         WIN_FLAG = "";
         UNIX_PATH = browser;
      }

      String os = System.getProperty("os.name");
      if ( os != null && os.startsWith(WIN_ID))
         windows = true;
      else
         windows = false;

      try
      {
          if (windows)
          {
              // cmd = 'rundll32 url.dll,FileProtocolHandler http://...'
              cmd = WIN_PATH + " " + WIN_FLAG + " " + url;
              Process p = Runtime.getRuntime().exec(cmd);
          }
          else
          {
              // Under Unix, Netscape has to be running for the "-remote"
              // command to work.  So, we try sending the command and
              // check for an exit value.  If the exit command is 0,
              // it worked, otherwise we need to start the browser.
              // cmd = 'netscape -remote openURL(http://www.javaworld.com)'
              cmd = UNIX_PATH + " " + UNIX_FLAG + "(" + url + ")";
              Process p = Runtime.getRuntime().exec(cmd);
              try
              {
                  // wait for exit code -- if it's 0, command worked,
                  // otherwise we need to start the browser up.
                  int exitCode = p.waitFor();
                  if (exitCode != 0)
                  {
                      // Command failed, start up the browser
                      // cmd = 'netscape http://www.javaworld.com'
                      cmd = UNIX_PATH + " "  + url;
                      p = Runtime.getRuntime().exec(cmd);
                  }
              }
              catch(InterruptedException ex)
              {
                 throw(ex);
              }
          }
      }
      catch(IOException ex)
      {
         throw(ex);
      }
   }

   public static Properties prologTextTableToProperties(LogicServer ls, long table) throws LSException
   {
      Properties p = new Properties();
      long list, element;
      String name, value;

      // Check for the empty list or an atom
      long type = ls.GetTermType(table);
      if (type != LogicServer.pLIST) return p;

      // Skip the first row with the column names
      table = ls.GetTail(table);

      // Get the rest of the rows into a Property list
      while (table != 0)
      {
         list = ls.GetHead(table);
         name = ls.GetStrHead(list);
         list = ls.GetTail(list);
         value = ls.GetStrHead(list);
         p.setProperty(name, value);

         table = ls.GetTail(table);
      }
      return p;
   }

   public static Properties prologListToProperties(LogicServer ls, long list, int size) throws LSException
   {
      Properties p = new Properties();
      long element, name, value;

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);
      if (type != LogicServer.pLIST) return p;

/*      // Otherwise get the head
      element = ls.GetHead(list);
      name = ls.GetArg(element, 1);
      value = ls.GetArg(element, 2);
      if (ls.GetTermType(value) == LogicServer.pLIST)
         p.setProperty(ls.TermToStr(name, size), ls.TermToStrQ(value, size));
      else
         p.setProperty(ls.TermToStr(name, size), ls.TermToStr(value, size));

      // And the rest of the list
      list = ls.GetTail(list);
*/
      while (list != 0)
      {
         element = ls.GetHead(list);
         name = ls.GetArg(element, 1);
         value = ls.GetArg(element, 2);
         if (ls.GetTermType(value) == LogicServer.pLIST)
            p.setProperty(ls.TermToStr(name, size), ls.TermToStrQ(value, size));
         else
            p.setProperty(ls.TermToStr(name, size), ls.TermToStr(value, size));

         list = ls.GetTail(list);
      }
      return p;
   }

   public static Properties prologListToPropertiesQ(LogicServer ls, long list, int size) throws LSException
   {
      Properties p = new Properties();
      long element, name, value;

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);
      if (type != LogicServer.pLIST) return p;

/*      // Otherwise get the head
      element = ls.GetHead(list);
      name = ls.GetArg(element, 1);
      value = ls.GetArg(element, 2);
      p.setProperty(ls.TermToStr(name, size), ls.TermToStrQ(value, size));

      // And the rest of the list
      list = ls.GetTail(list);
*/
      while (list != 0)
      {
         element = ls.GetHead(list);
         name = ls.GetArg(element, 1);
         value = ls.GetArg(element, 2);
         p.setProperty(ls.TermToStr(name, size), ls.TermToStrQ(value, size));

         list = ls.GetTail(list);
      }
      return p;
   }

   public static Vector prologListToNameValueVector(LogicServer ls, long list, int size) throws LSException
   {
      Vector v = new Vector();
      long element, name, value;

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);

      if (type != LogicServer.pLIST) return v;

      while (list != 0)
      {
         element = ls.GetHead(list);
         name = ls.GetArg(element, 1);
         value = ls.GetArg(element, 2);
         v.addElement(ls.TermToStr(name, size) + " = " + ls.TermToStrQ(value, size));

         list = ls.GetTail(list);
      }
      return v;
   }

   public static Vector prologListToVector(LogicServer ls, long list, int size) throws LSException
   {
      Vector v = new Vector();

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);
//      String foo = ls.TermToStr(list, size);

      if (type != LogicServer.pLIST) return v;

/*      // Otherwise get the head
//      type = ls.GetTermType(ls.GetHead(list));
      v.addElement(ls.TermToStr(ls.GetHead(list), size));

      // And the rest of the list
      list = ls.GetTail(list);
*/
      while (list != 0)
      {
//         type = ls.GetTermType(ls.GetHead(list));
         v.addElement(ls.TermToStr(ls.GetHead(list), size));

         list = ls.GetTail(list);
      }
      return v;
   }

   public static Vector prologListToVectorQ(LogicServer ls, long list, int size) throws LSException
   {
      Vector v = new Vector();

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);
      if (type != LogicServer.pLIST) return v;

/*      // Otherwise get the head
      type = ls.GetTermType(ls.GetHead(list));
      v.addElement(ls.TermToStrQ(ls.GetHead(list), size));

      // And the rest of the list
      list = ls.GetTail(list);

*/
      while (list != 0)
      {
         type = ls.GetTermType(ls.GetHead(list));
//         if (type == 2 /*string*/ || type == 9 /*widestring*/)
//            v.addElement("\"" + ls.TermToStr(ls.GetHead(list), size) + "\"");
//         else
            v.addElement(ls.TermToStrQ(ls.GetHead(list), size));

         list = ls.GetTail(list);
      }
      return v;
   }

   public static String findClasspathFile(String fileName, String relDir)
   {
      SearchPath sp;
      String path;
      File f;

      try
      {
         sp = new SearchPath(System.getProperty("java.class.path"), relDir);
         path = sp.getPath(fileName);
         f = new File(path);
         if (path != null && path.length() > 0 && f.exists())
            return f.getCanonicalPath();

         sp = new SearchPath(System.getProperty("user.dir"), relDir);
         path = sp.getPath(fileName);
         f = new File(path);
         if (path != null && path.length() > 0 && f.exists())
            return f.getCanonicalPath();
      }
      catch (IOException ex)
      {
      }

      return "";
   }

   public static String getObjectName(String text, int cursor, char delimiter)
   {
      int firstDelimiter, lastDelimiter;

      if (cursor < 0) return null;
      if (text == null) return null;

      // Find the delimiter before and after the cursor
      firstDelimiter = cursor;
      while (firstDelimiter >= 0)
      {
         if (text.charAt(firstDelimiter) == delimiter) break;
         firstDelimiter = firstDelimiter - 1;
      }
      // Space is a special case, start of line is okay
      if (firstDelimiter < 0 && delimiter != ' ') return null;

      lastDelimiter = cursor;
      while (lastDelimiter < text.length())
      {
         if (text.charAt(lastDelimiter) == delimiter) break;
         lastDelimiter = lastDelimiter + 1;
      }
      // Space is a special case, end of line is okay
      if (lastDelimiter == text.length() && delimiter != ' ') return null;

      return text.substring(firstDelimiter+1, lastDelimiter);
   }

   public static String doubleSlashes(String s)
   {
      String slashed;
      int i, slash;

      slashed = "";
      i = 0;
      while ((slash = s.indexOf("\\", i)) >= 0)
      {
         slashed = slashed + s.substring(i, slash) + "\\\\";
         i = slash+1;
      }
      slashed = slashed + s.substring(i, s.length());
      return slashed;
   }

   public static String doubleDollars(String in)
   {
      StringBuffer sb = new StringBuffer("");
      for (int i = 0; i < in.length(); i++)
      {
         if (in.charAt(i) == '$')
            sb.append(in.charAt(i));
         sb.append(in.charAt(i));
      }
      return sb.toString();
   }

   public static String doubleDoubleQuotes(String in)
   {
      StringBuffer sb = new StringBuffer("");
      for (int i = 0; i < in.length(); i++)
      {
         if (in.charAt(i) == '"')
            sb.append(in.charAt(i));
         sb.append(in.charAt(i));
      }
      return sb.toString();
   }

   public static String doubleQuotes(String in)
   {
      // Return if it already starts and ends with a single quote
      if (in.startsWith("'") && in.endsWith("'")) return in;

      StringBuffer sb = new StringBuffer("");
      for (int i = 0; i < in.length(); i++)
      {
         if (in.charAt(i) == '\'')
            sb.append(in.charAt(i));
         sb.append(in.charAt(i));
      }
      return sb.toString();
   }

   public static String quotePathname(String pathname)
   {
      int i, j, k;

      if (pathname.trim().equals("/")) return "/";

      String result = "";
      i = pathname.indexOf('/');
      do
      {
         i++;
         while (pathname.charAt(i) == 32) i++;
         j = pathname.indexOf('/', i);
         if (j < 0) j = pathname.length();
         k = j;
         while (k > 0 && pathname.charAt(k-1) == 32) k--;
//         int first = pathname.charAt(i);
//         int last = pathname.charAt(j-1);
         if (pathname.charAt(i) == '\'' && pathname.charAt(k-1) == '\'')
            result = result + "/ " + pathname.substring(i, k);
         else
         {
            String element = pathname.substring(i, k).trim();
            result = result + "/ '" + doubleQuotes(element) + "'";
         }
         i = j;
      } while (j < pathname.length());
      return result;
   }

   public static Vector formatPaths(Vector v)
   {
      String path;
      StringBuffer sb;
      Vector w = new Vector(v.size());

      for (int n = 0; n < v.size() ; n++)
      {
         path = (String)v.elementAt(n);
         sb = new StringBuffer("");
         for (int i = 0; i < path.length(); i++)
         {
            if (path.charAt(i) == '/')
            {
               // Make sure there's a space before each slash
               // (except at beginning)
               if (i != 0 && path.charAt(i-1) != ' ')
                  sb.append(' ');

               sb.append(path.charAt(i));

               // And a space after each slash
               if (i < path.length()-1 && path.charAt(i+1) != ' ')
                  sb.append(' ');
            }
            else
               sb.append(path.charAt(i));
         }

         w.addElement(sb.toString());
      }
      return w;
   }


   public static String extractPath(String pathname)
   {
      int i = pathname.lastIndexOf('/');
      if (i == 0)
         return "/";
      else
         return pathname.substring(0, i-2);
   }

   public static String extractName(String pathname)
   {
      int i = pathname.lastIndexOf('/');
      return pathname.substring(i+2, pathname.length()-1);
   }

}
