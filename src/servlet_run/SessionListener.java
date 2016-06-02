/**
 * Title:        KnowledgeWright Servlet Interface
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Amzi! inc.
 * @author
 * @version 1.0
 */

import amzi.ls.*;
import javax.servlet.http.*;

public class SessionListener implements HttpSessionListener
{

   public SessionListener()
   {
   }

   public void sessionCreated(HttpSessionEvent se)
   {
   }

   public void sessionDestroyed(HttpSessionEvent se)
   {
      HttpSession session = se.getSession();
      LogicServer ls = (LogicServer)session.getAttribute("kw.logicserver");

      // Remove all our attributes
      session.removeAttribute("kw.logicserver");
      session.removeAttribute("kw.charset");
      session.removeAttribute("kw.kb_file");

      if (ls != null)
         try
         {
            ls.Close();
         }
         catch (LSException ex)
         {
         }
   }


}