// KnowledgeWright Java Runtime Prototype
// Copyright (c)2001 Amzi! inc. All Rights Reserved.
//
// BEFORE RUNNING THIS PROGRAM, the following files in the 'workshop' subdirectory
// must either be on the system environment PATH variable, or copied to the same
// directory as this program: amzi.dll, aosutils.lsx, aodbc.lsx, amzi.cfg
// Also you need to copy the jig reasoning engine (.xpl) file from the 'workshop/jigs'
// subdirectory (e.g. basic.xpl for the basic jig), and a knowledgebase file (.kb)
// from the samples subdirectory.
//

import amzi.ls.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

public class mainFrame extends JFrame
{
   JPanel contentPane;
   BorderLayout mainBorderLayout = new BorderLayout();
   JPanel buttonPanel = new JPanel();
   JButton runButton = new JButton();
   JScrollPane textScroller = new JScrollPane();
   JEditorPane outputText = new JEditorPane();

   LogicServer ls;
   JButton exitButton = new JButton();

   // Construct the frame
   public mainFrame()
   {
      enableEvents(AWTEvent.WINDOW_EVENT_MASK);
      try
      {
         jbInit();
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   // Component initialization
   private void jbInit() throws Exception
   {
      contentPane = (JPanel) this.getContentPane();
      contentPane.setLayout(mainBorderLayout);
      this.setSize(new Dimension(600, 400));
      this.setTitle("KnowledgeWright Java Runtime Sample");
      runButton.setActionCommand("runButton");
      runButton.setText("Run");
      runButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            runButton_actionPerformed(e);
         }
      });
      exitButton.setText("Exit");
      exitButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            exitButton_actionPerformed(e);
         }
      });
      contentPane.add(buttonPanel, BorderLayout.NORTH);
      buttonPanel.add(runButton, null);
      buttonPanel.add(exitButton, null);
      contentPane.add(textScroller, BorderLayout.CENTER);
      outputText.setContentType("text/html");
      outputText.getEditorKit().createDefaultDocument();
      outputText.setEditable(false);
      textScroller.getViewport().add(outputText, null);
   }

   // This is the main function. It loads and runs a knowledgebase using the KWI.
   // The KWI is a Prolog language calling interface to KnowledgeWright, so all
   // the KWI calls are made through the Amzi! Logic Server.
   void runButton_actionPerformed(ActionEvent e)
   {
      long solveTerm, actionTerm, responseTerm, term;
      String id = "none", responseFunctor, responseType, solutions = "", s;
      boolean more = true, oneAnswer;

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
         ls.AddLSX("aodbc", 0);

         // Load the Jig
         ls.Load("basic.xpl");

         // Initialize the runtime with directory and logfile
         // Prior to starting a new KWI session the session id must be kw_init.
         String path = System.getProperty("user.dir") + System.getProperty("file.separator");
         id = "kw_init";

         // The directory is where the kb file is located
         // The session_directory is where a temporary file is created to maintain the state
         //   of the reasoning process
         // The log_file is a log of the reasoning process and is created in the directory
         // The message level is the level of detail in the log_file
         s = "kwi(" + id + ", initialize([directory = $" + Utils.doubleSlashes(path) + "$, " +
                    "session_directory = $" + Utils.doubleSlashes(path) + "$, " +
//                    "message_level = none]), _INFO)";
                    "log_file = $kwrun.log$, message_level = high]), _INFO)";
         term = ls.ExecStr(s);
         if (term == 0)
            JOptionPane.showMessageDialog(null, "kwi / initialize failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // Get the name, version and build (optional)
         Properties sysInfo = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 5000);
         String engineName = sysInfo.getProperty("system");
         String version = sysInfo.getProperty("version");
         outputText.setText("System: " + engineName + " Version: " + version);

         // Open the logicbase
         String name = JOptionPane.showInputDialog("Enter name of knowledgebase to open:");
         if (name.length() == 0) return;
         s = "kwi(" + id + ", open($" + name + "$), _)";
         term = ls.ExecStr(s);
         if (term == 0)
            JOptionPane.showMessageDialog(null, "kwi / open " + name + " failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // Start a new session
         // Now the id can be anything but kw_init
         id = "session_0";
         if (ls.ExecStr("kwi('" + id + "', new_session, _)") == 0 )
            JOptionPane.showMessageDialog(null, "kwi / new_session failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // Keep calling kwi / solve until there is nothing else to do
         while((solveTerm = ls.ExecStr("kwi('" + id + "', solve, _MORE)")) != 0)
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
               {
                  JOptionPane.showMessageDialog(null, "kwi / get_action",
                     "Error", JOptionPane.ERROR_MESSAGE);
                     return;
               }

               // The third parameter in the kwi call contains the type and
               // parameters of the action to be performed
               // Use the Amzi! Logic Server to retrieve the action
               responseTerm = ls.GetArg(actionTerm, 3);
               responseFunctor = ls.GetFunctor(responseTerm);
               responseType = ls.GetStrArg(responseTerm, 1);

               // Display the HTML question page (we ignore this...used by the web interface)
               if (responseFunctor.equals("ask") && responseType.equals("html"))
               {
                  // Do nothing
               }

               // Get a fact from the user
               if (responseFunctor.equals("ask") && responseType.equals("user"))
               {
                  String factName, prompt, questionType, defaultValue, answerType, delimiter;
                  int length;
                  Vector menu;

                  // The name of the fact we are seeking a value for
                  factName = ls.GetStrArg(responseTerm, 2);

                  // Get all the slots from the question object into a Properties object
                  Properties question = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 3), 5000);

                  // Get the prompt
                  prompt = question.getProperty("prompt");

                  // Get the question type
                  questionType = question.getProperty("question_type");

                  // Handle fill in the blank separate from menu choices
                  if (questionType.equals("fill_in_the_blank"))
                  {
                     // Get the field length
                     length = new Integer((String)question.getProperty("length")).intValue();

                     // Get the default value
                     defaultValue = question.getProperty("default");

                     // Ask the user
                     AskFieldDialog ask = new AskFieldDialog(this, "Question: " + factName, prompt, defaultValue, length);
                     Point loc = this.getContentPane().getLocationOnScreen();
                     ask.setLocation(loc.x, loc.y);
                     ask.setModal(true);
                     ask.show();

                     // See if the user wants to stop
                     if (ask.getValue().length() == 0) return;

                     // Assert the new fact value using the KWI
                     answerType = question.getProperty("answer_type");

                     // Assert everything as strings, the KWI will convert
                     delimiter = "$";

                     if (ls.ExecStr("kwi('" + id + "', assert(fact('" + factName + "', " + delimiter + ask.getValue() + delimiter + ")), _)") == 0)
                        JOptionPane.showMessageDialog(null, "kwi / assert failed",
                          "Error", JOptionPane.ERROR_MESSAGE);
                  }
                  // Menu single and multiple choice
                  else
                  {
                     // Get all the menu choices into a vector
                     // Note this code will not work if you have a separate display string
                     menu = Utils.prologListToVector(ls, ls.StrToTerm(question.getProperty("choices")), 5000);
                     if (question.getProperty("question_type").equals("menu_multiple_choices"))
                        oneAnswer = false;
                     else
                        oneAnswer = true;

                     // Ask the user
                     AskMenuDialog ask = new AskMenuDialog(this, "Fact: " + factName, prompt, menu, oneAnswer);
                     Point loc = this.getContentPane().getLocationOnScreen();
                     ask.setLocation(loc.x, loc.y);
                     ask.setModal(true);
                     ask.show();

                     // See if the user wants to stop
                     if (ask.getSelectedValue() == null) return;

                     // Assert the new fact or facts (for multiple choices)
                     if (oneAnswer)
                        s = "kwi('" + id + "', assert(fact('" + factName + "', $" + ask.getSelectedValue() + "$)), _)";

                     // Multiple answers need to be asserted as a prolog list which is enclosed in []'s
                     else
                     {
                        // Build the list from the vector returned from AskMenuDialog
                        Vector facts = ask.getSelectedValues();
                        String factList = "";
                        for (int i = 0 ; i < facts.size() ; i++)
                           factList = factList + "$" + facts.elementAt(i) + "$,";
                        s = "kwi('" + id + "', assert(fact('" + factName + "', [" + factList.substring(0,factList.length()-1) + "])), _)";
                     }
                     if (ls.ExecStr(s) == 0)
                        JOptionPane.showMessageDialog(null, "kwi / assert",
                           "Error", JOptionPane.ERROR_MESSAGE);
                  }
               }

               // Display an answer
               if (responseFunctor.equals("tell") && responseType.equals("user"))
               {
                  // Get the type of the answer
                  String answerType = ls.GetStrArg(responseTerm, 1);

                  // Get all the slots in the answer into a Properties object
                  Properties answer = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 2), 100000);

                  // Get the name of the answer
                  String answerName = answer.getProperty("goal");

                  // Add new answer onto existing ones
                  String newSolution = answer.getProperty("text");
                  if (newSolution != null)
                  {
                     solutions = solutions + "<P>" + answerName + ": " + newSolution + "</P>";
                     outputText.setText(solutions);
                  }
                  else
                     JOptionPane.showMessageDialog(null, "No text for goal: " + answerName,
                        "Error", JOptionPane.ERROR_MESSAGE);
               }

            }
            while (!responseFunctor.equals("none"));

            if (!more)  break;

         } // while solveTerm

         ls.Close();
      }
      catch (LSException ex)
      {
         displayKBError(ex, "'" + id + "'");
      }

   }

   // Display errors from the KWI or the underlying Amzi! Logic Server
   public void displayKBError(LSException e, String id)
   {
      String s;
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
               JOptionPane.showMessageDialog(this, "Unable to call kwi / get_error for: " + e.GetMsg(),
                  "Logic Server Error", JOptionPane.ERROR_MESSAGE);
               return;
            }

            // Get all the attributes of the error into a Properties object
            Properties info = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 10000);
            Enumeration propNames = info.propertyNames();

            // Build a nicely formatted message
            String msg = "";
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
            JOptionPane.showMessageDialog(this, msg,
               "Logicbase Execution Error", JOptionPane.ERROR_MESSAGE);
         }
         // Consult errors (outside the KWI)
         else if (e.GetType() == LSException.READ /*read*/)
         {
            String lineno = new Integer(e.GetLineno()).toString();
            String msg = e.GetMsg() + "\n in file " + e.GetReadFileName() +
               "\n at line " + lineno + "\n" + e.GetReadBuffer();
            JOptionPane.showMessageDialog(this, msg,
               "Syntax Error", JOptionPane.ERROR_MESSAGE);
          }

         // Unrecognized Logic Server error
         else
            JOptionPane.showMessageDialog(this, e.GetMsg(),
               "Logic Server Error", JOptionPane.ERROR_MESSAGE);
      }
      catch (LSException e2)
      {
         System.out.println("Error catching error");
         e.printStackTrace();
      }
   }

   // The exit button
   void exitButton_actionPerformed(ActionEvent e)
   {
      System.exit(0);
   }

   // Overridden so we can exit when window is closed
   protected void processWindowEvent(WindowEvent e)
   {
      super.processWindowEvent(e);
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
         System.exit(0);
      }
   }

}
