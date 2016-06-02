
//Title:        SupportKT IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import amzi.ls.*;

public class KBException extends Exception
{
   String message;

   public KBException(String msg)
   {
      super(msg);
      message = msg;
   }

   public KBException(LSException e)
   {
      super();

      // Consult errors
      if (e.GetType() == 8 /*read*/)
      {
         String lineno = new Integer(e.GetLineno()).toString();
         String msg = e.GetMsg() + "\n" + " in file " + e.GetReadFileName() +
            "\n" + " at line " + lineno + "\n" + e.GetReadBuffer();
         message = "Syntax Error " + msg;
      }
      // Other Logic Server error
      else
      {
         message = "Logic Server Error " + e.GetMsg();
      }
   }

   public String getMessage()
   {
      return message;
   }

}