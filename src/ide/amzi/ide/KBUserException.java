
//Title:        SupportKT IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import amzi.ls.*;
import java.util.*;

public class KBUserException extends Exception
{
   /**
   * Class defining preformatted error messages.
   */
   static class ErrorMsg
   {
      public int number;
      public int type;
      public String msg;

      /**
      * Full argument constructor.
      * @param n - Message number.
      * @param t - Message type.
      * @param m - Message string.
      */
      public ErrorMsg(int n, int t, String m)
      {
         number = n;
         type = t;
         msg = m;
      }
   }

   // Types of errors
   public final static int UNKNOWN  = 0;
   public final static int FATAL    = 1;
   public final static int INTERNAL = 2;
   public final static int ERROR    = 3;

   // KB Level Errors 101-199
   public final static int CANNOT_OPEN          = 101;
   public final static int CANNOT_SAVE          = 102;
   public final static int CANNOT_SAVE_AS       = 103;
   public final static int CANNOT_RENAME_OBJECT = 104;
   public final static int CANNOT_DELETE_OBJECT = 105;
   public final static int CANNOT_MOVE_OBJECT   = 106;
   public final static int CANNOT_INIT          = 107;
   public final static int CANNOT_LOAD_SCHEMA   = 108;
   public final static int CANNOT_CONVERT       = 109;
   public final static int CANNOT_FIND_JIG      = 110;
   public final static int CANNOT_CREATE        = 111;

   // KB Frame Level Errors 201-299
   public final static int CANNOT_CREATE_OBJECT = 201;
   public final static int CANNOT_OPEN_OBJECT   = 202;
   public final static int CANNOT_GET_SLOT      = 203;
   public final static int CANNOT_GET_SLOT_TYPE = 204;
   public final static int CANNOT_SET_SLOT      = 205;
   public final static int BAD_NUMBER_SLOT_VALUE= 206;
   public final static int BAD_TERM_SLOT_VALUE  = 207;
   public final static int DUPLICATE_NAME       = 208;
   public final static int EMPTY_TERM_SLOT_VALUE= 209;

   // Prolog Interface Errors 301-399
   public final static int PREDICATE_FAILED     = 301;

   // Java Errors 900-999
   public final static int NO_JAVA_CLASS_NAME   = 901;


   protected boolean dumpExceptions = true;

   private static ErrorMsg[] msgs =
   {
      /*101*/ new ErrorMsg(CANNOT_OPEN, ERROR,
         "Unable to open knowledgebase file: "),
      /*102*/ new ErrorMsg(CANNOT_SAVE, ERROR,
         "Unable to save knowledgebase file: "),
      /*103*/ new ErrorMsg(CANNOT_SAVE_AS, ERROR,
         "Unable to save knowledgebase as file: "),
      /*104*/ new ErrorMsg(CANNOT_RENAME_OBJECT, ERROR,
         "Unable to rename object: "),
      /*105*/ new ErrorMsg(CANNOT_DELETE_OBJECT, ERROR,
         "Unable to delete object: "),
      /*106*/ new ErrorMsg(CANNOT_MOVE_OBJECT, ERROR,
         "Unable to move object: "),
      /*107*/ new ErrorMsg(CANNOT_INIT, ERROR,
         "Unable to initialize knowledgebase"),
      /*108*/ new ErrorMsg(CANNOT_LOAD_SCHEMA, ERROR,
         "Unable to find or load knowledgebase jig"),
      /*109*/ new ErrorMsg(CANNOT_CONVERT, ERROR,
         "Unable to convert knowledgebase named: "),
      /*110*/ new ErrorMsg(CANNOT_FIND_JIG, ERROR,
         "Unable to find jig for knowledgbase"),
      /*111*/ new ErrorMsg(CANNOT_CREATE, ERROR,
         "Unable to create new knowledgebase: "),

      /*201*/ new ErrorMsg(CANNOT_CREATE_OBJECT, ERROR,
         "Unable to create new object of type: "),
      /*202*/ new ErrorMsg(CANNOT_OPEN_OBJECT, ERROR,
         "Unable to open object named: "),
      /*203*/ new ErrorMsg(CANNOT_GET_SLOT, ERROR,
         "Unable to get value for property: "),
      /*204*/ new ErrorMsg(CANNOT_GET_SLOT_TYPE, ERROR,
         "Unable to get type of property: "),
      /*205*/ new ErrorMsg(CANNOT_SET_SLOT, ERROR,
         "Unable to set field: "),
      /*206*/ new ErrorMsg(BAD_NUMBER_SLOT_VALUE, ERROR,
         "Invalid number value in property: "),
      /*207*/ new ErrorMsg(BAD_TERM_SLOT_VALUE, ERROR,
         "Invalid value in property: "),
      /*208*/ new ErrorMsg(DUPLICATE_NAME, ERROR,
         "A folder or object already exists with name: "),
      /*209*/ new ErrorMsg(EMPTY_TERM_SLOT_VALUE, ERROR,
         "Value cannot be blank in property: "),

      /*301*/ new ErrorMsg(PREDICATE_FAILED, INTERNAL,
         "Predicate failed: "),

      /*901*/ new ErrorMsg(NO_JAVA_CLASS_NAME, INTERNAL,
         "Unable to get Java class name for class: "),

      new ErrorMsg(UNKNOWN, UNKNOWN,
         "Unknown Knowledgebase User Error")
   };

   protected String prefix = "";
   protected String msg = "";
   protected int errorNumber = 0;

   public KBUserException()
   {
      msg = "";
      prefix = "";
      errorNumber = 0;
      serverInfo();
   }

   /**
    * Constructor for exception with just a preformatted message.
    *
    * @param   i  the identifier of the message
    */
   public KBUserException(int i)
   {
      msg = findMsg(i);
      prefix = "";
      errorNumber = i;
      serverInfo();
   }

   /**
    * Constructor for exception with preformatted message and
    * suffix.
    *
    * @param   i  the identifier of the message
    * @param   suffix  the suffix
    */
   public KBUserException(int i, String suffix)
   {
      msg = findMsg(i) + suffix;
      prefix = "";
      errorNumber = i;
      serverInfo();
   }


   /**
    * Constructor for exception with a custom message.
    *
    * @param   m  the message to be used
    */
   public KBUserException(String m)
   {
      msg = m;
      prefix = "";
      errorNumber = 0;
      serverInfo();
   }

   /**
    * Constructor for exception with a preformatted message
    * and a customized message prefix.
    *
    * @param   c  the message prefix
    * @param   i  the message identifier
    */
   public KBUserException(String c, int i)
   {
      msg = findMsg(i);
      prefix = c;
      errorNumber = i;
      serverInfo();
   }

   /**
    * Constructor for exception with a preformatted
    * message and a prefix generated from the class
    * name of the thrower.
    *
    * @param   o  an object, usually the one that threw
    *             the message
    * @param   i  the message identifier
    */
   public KBUserException(Object o, int i)
   {
      msg = findMsg(i);
      prefix = o.getClass().getName();
      errorNumber = i;
      serverInfo();
   }

   /**
    * Constructor for exception with a custom
    * message and a prefix generated from the class
    * name of the thrower.
    *
    * @param   o  an object, usually the one that threw
    *             the message
    * @param   m  the custom message
    */
   public KBUserException(Object o, String m)
   {
      msg = m;
      prefix = o.getClass().getName();
      errorNumber = 0;
      serverInfo();
   }

   /**
    * Constructor for exception with a preformatted
    * message, a prefix generated from the class
    * name of the thrower, and additional information.
    *
    * @param   o  an object, usually the one that threw
    *             the message
    * @param   i  the message identifier
    * @param   a  additional information
    */
   public KBUserException(Object o, int i, String a)
   {
      super();
      msg = findMsg(i) + a;
      prefix = o.getClass().getName();
      errorNumber = i;
      serverInfo();
   }

   /**
    * Constructor for exception with a Logic Server
    * exception.
    *
    * @param   e  the logic server exception
    */
   public KBUserException(LSException e)
   {
      super();

      // Consult errors
      if (e.GetType() == 8 /*read*/)
      {
         if (e.GetReadFileName().length() > 0)
         {
            int line = e.GetLineno();
            String lineno = new Integer(line).toString();
            msg = e.GetMsg() + "\n" + " in file " + e.GetReadFileName() +
               "\n" + " at line " + lineno + "\n" + e.GetReadBuffer();
         }
         else
            msg = e.GetMsg() + "\n" + e.GetReadBuffer();
         msg = "Syntax Error: " + msg;
      }
      // Other Logic Server error
      else
      {
         msg = "Logic Server Error: " + e.GetMsg();
      }

      prefix = "";
      errorNumber = 0;
      serverInfo();
   }

   /**
    * Constructor for exception with a Logic Server
    * exception and a customized message prefix.
    *
    * @param   c  the message prefix
    * @param   e  the logic server exception
    */
   public KBUserException(String c, LSException e)
   {
      super();

      // Consult errors
      if (e.GetType() == 8 /*read*/)
      {
         if (e.GetReadFileName().length() > 0)
         {
            int line = e.GetLineno();
            String lineno = new Integer(line).toString();
            msg = e.GetMsg() + "\n" +
               " in file " + e.GetReadFileName() + "\n" +
               " at line " + lineno + "\n" +
               e.GetReadBuffer();
         }
         else
            msg = e.GetMsg() + "\n" + e.GetReadBuffer();
         msg = "Syntax Error: " + msg;
      }
      // Other Logic Server error
      else
      {
         msg = "Logic Server Error: " + e.GetMsg();
      }

      prefix = c;
      errorNumber = 0;
      serverInfo();
   }

   /**
    * Constructor for a syntax error thrown by
    * checkSlotValue
    *
    * @param   i  the message identifier
    * @param   p  the properties of the error
    */
   public KBUserException(Properties p)
   {
      super();

      if (p.getProperty("type") != null &&  p.getProperty("type").equals("read"))
      {
         msg = "Syntax Error: " + p.getProperty("message") + "\n";
         msg = msg + "Property: " + p.getProperty("slot") + "\n" +
            "Invalid Value: " + p.getProperty("read_buffer");
      }
      else
         msg = "Error: " + p.getProperty("message");
   }

   /**
   * Provide information on the exception for the server.
   */
   protected void serverInfo()
   {
      if (dumpExceptions)
      {
         System.out.println();
         System.out.println("***** Creating KBUserException *****");
         System.out.println("  prefix = " + prefix);
         System.out.println("  msg = " + msg);
         System.out.println("Stack Trace:");
         Thread.currentThread().dumpStack();
//         System.out.prinln(Thread.currentThread().getName());
         System.out.println("********* Exception Created ********");
         System.out.println();
      }
   }

   protected String findMsg(int errorNumber)
   {
      // Just walk down the array looking, as this only
      // happens occasionally, its not such a
      // big deal.
      int i;
      for (i=0; i<msgs.length-1; i++)  // last message is default
      {
         if (msgs[i].number == errorNumber)
            break;
      }
      return msgs[i].msg;
   }

   protected String findType(int errorNumber)
   {
      int i;
      for (i=0; i<msgs.length-1; i++)  // last message is default
      {
         if (msgs[i].number == errorNumber)
            break;
      }
      switch (msgs[i].type)
      {
         case UNKNOWN: return "Unknown Error";
         case FATAL: return "Fatal Error";
         case ERROR: return "User Error";
         case INTERNAL: return "Internal Error";
      }
      return "";
   }

   public int getErrorNumber() { return errorNumber; }

   public String getErrorType() { return findType(errorNumber); };

   public String getMessage()
   {
      if (errorNumber != 0)
         return "Error #" + errorNumber + ": " + prefix + " " + msg;
      else if (prefix != null && prefix.length() > 0)
         return prefix + " " + msg;
      else
         return msg;
   }

}