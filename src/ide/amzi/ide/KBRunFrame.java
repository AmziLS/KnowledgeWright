
//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:


package  amzi.ide;

import amzi.ls.*;
import com.borland.jb.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import java.io.*;
import java.lang.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import javax.swing.border.*;

public class KBRunFrame extends JInternalFrame
{
   BorderLayout borderLayout = new BorderLayout();
   JScrollPane stateScroller = new JScrollPane();
   JPanel buttonPanel = new JPanel();
   JButton stepButton = new JButton();
   JButton runButton = new JButton();
   JButton printLogButton = new JButton();
   JButton exitButton = new JButton();
   JSplitPane jSplitPane1 = new JSplitPane();
   JScrollPane logScroller = new JScrollPane();
   JTextArea logTextArea = new JTextArea();
   JTextArea stateTextArea = new JTextArea();

   App app;
   HTMLFrame htmlPane;
   LogicServer ls;
   String id, logName;
   Cursor lastCursor;
   String solutions = "";
   String runXPL, logLevel, initialDir;
   File htmlOutput;
   BufferedWriter htmlFileWriter;

   public KBRunFrame(App app, File kb, HTMLFrame html, String initialDir, String runXPL, boolean singleStep, String logLevel, boolean loadODBC)
   {
      super("Running " + kb.getName(), true, true, true, true);
      this.app = app;
      this.htmlPane = html;
      this.runXPL = runXPL;
      this.logLevel = logLevel;
      this.initialDir = initialDir;

      try
      {
         jbInit();
         this.pack();
         this.show();
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
      try
      {
         // Load the WebLS runtime
         ls = new LogicServer();
         ls.Init(app.initialDir + "amzi.cfg");
         ls.AddLSX("aosutils", 0);
         if (loadODBC)
            ls.AddLSX("aodbc", 0);
         String xplPath = Utils.findClasspathFile(runXPL, "jigs");
         ls.Load(xplPath);

         // Initialize the runtime with directory and logfile
         String path = kb.getParent() + File.separator;
         id = "kw_init"; // (new Integer(this.hashCode())).toHexString(this.hashCode());
         logName = Utils.doubleSlashes(app.tempDirectory) + "kwrun.log";

         // debug_mgs should be local for production, and message_level is from logLevel
         String s = "kwi(" + id + ", initialize([directory = $" + Utils.doubleSlashes(path) + "$, " +
                    "session_directory = $" + Utils.doubleSlashes(app.tempDirectory) + "$, " +
//                    "log_file = $kwrun.log$, debug_msgs = log_file, message_level = max]), _VAR_INFO)";
                    "log_file = $kwrun.log$, debug_msgs = local, message_level = " + logLevel + "]), _VAR_INFO)";

         long term = ls.ExecStr(s);
         if (term == 0)
            JOptionPane.showMessageDialog(null, "kwi / initialize failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // Get the name, version and build
         Properties info = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 5000);
         String engineName = info.getProperty("system");
         String version = info.getProperty("version");
         stateTextArea.setText(ls.GetVersion() + "\n" + engineName + " version " + version);

         // Open the logicbase
         String name = kb.getName();
         String kbcName = name.substring(0, name.lastIndexOf('.')) + ".kbc";
         File kbc = new File(kb.getParent(), kbcName);
         if (kbc != null && kbc.lastModified() > kb.lastModified())
            name = kbcName;
         s = "kwi(" + id + ", open($" + name + "$), _)";
         term = ls.ExecStr(s);
         if (term == 0)
            JOptionPane.showMessageDialog(null, "kwi / open " + name + " failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // Start a new session
         id = "session_0";
         if (ls.ExecStr("kwi('" + id + "', new_session, _)") == 0 )
            JOptionPane.showMessageDialog(null, "kwi / new_session failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         // And run if we're running
         if (!singleStep) kbLoop(false);
      }
      catch (LSException e)
      {
         displayKBError(e, id);
      }
   }

   public void jbInit() throws Exception
   {
      // Catch window closing
      addInternalFrameListener(new kbRunFrameListener());
      this.setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
      this.setVisible(true);
      this.setClosable(false);
      this.setIconifiable(true);
      this.setPreferredSize(new Dimension(400, 600));
      this.setMaximizable(true);
      this.setResizable(true);
      this.getContentPane().setLayout(borderLayout);

      stepButton.setIcon(new ImageIcon(amzi.ide.KBRunFrame.class.getResource("resources/step.gif")));
      stepButton.setText("Step");
      stepButton.setMnemonic(KeyEvent.VK_S);
      stepButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            stepButton_actionPerformed(e);
         }
      });

      runButton.setIcon(new ImageIcon(amzi.ide.KBRunFrame.class.getResource("resources/run.gif")));
      runButton.setText("Run");
      runButton.setMnemonic(KeyEvent.VK_R);
      runButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            runButton_actionPerformed(e);
         }
      });

      printLogButton.setIcon(new ImageIcon(amzi.ide.KBRunFrame.class.getResource("resources/print.gif")));
      printLogButton.setText("Print");
      printLogButton.setMnemonic(KeyEvent.VK_P);
      printLogButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            printLogButton_actionPerformed(e);
         }
      });

      exitButton.setIcon(new ImageIcon(amzi.ide.KBRunFrame.class.getResource("resources/stop.gif")));
      exitButton.setText("Exit");
      exitButton.setMnemonic(KeyEvent.VK_X);
      exitButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            exitButton_actionPerformed(e);
         }
      });

      buttonPanel.setBorder(BorderFactory.createEtchedBorder());
      buttonPanel.setLayout(new FlowLayout());
      buttonPanel.add(stepButton, null);
      buttonPanel.add(runButton, null);
      buttonPanel.add(printLogButton, null);
      buttonPanel.add(exitButton, null);

      stateTextArea.setForeground(Color.blue);
      stateTextArea.setFont(app.getUserFont());
      stateTextArea.setWrapStyleWord(true);
      stateTextArea.setLineWrap(true);
      stateTextArea.setEditable(false);
      stateTextArea.setMaximumSize(new Dimension(4096, 2048));
      stateScroller.setMaximumSize(new Dimension(4096, 2048));
      stateScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      stateScroller.getViewport().add(stateTextArea, null);

//      logTextArea.setWrapStyleWord(true);
//      logTextArea.setLineWrap(true);
      logTextArea.setEditable(false);
      logTextArea.setFont(app.getUserFont());
      logTextArea.setMaximumSize(new Dimension(4096, 2048));
      logScroller.setMaximumSize(new Dimension(4096, 2048));
      logScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      logScroller.getViewport().add(logTextArea, null);

      jSplitPane1.setOrientation(JSplitPane.VERTICAL_SPLIT);
      jSplitPane1.setBorder(null);
      jSplitPane1.setMaximumSize(new Dimension(4096, 4096));
      jSplitPane1.add(stateScroller, JSplitPane.TOP);
      jSplitPane1.add(logScroller, JSplitPane.BOTTOM);
//mm      Dimension frSize = jSplitPane1.getSize();
      jSplitPane1.setDividerLocation(200);

      this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
      this.getContentPane().add(jSplitPane1, BorderLayout.CENTER);
   }

   // Run
   void runButton_actionPerformed(ActionEvent e)
   {
      kbLoop(false);
   }

   // Step
   void stepButton_actionPerformed(ActionEvent e)
   {
      kbLoop(true);
   }

   // Print Log
   void printLogButton_actionPerformed(ActionEvent e)
   {
      // Get a PrinterJob
      PrinterJob job = PrinterJob.getPrinterJob();

      // Put up the dialog box
      if (job.printDialog())
      {
         // Ask user for page format (e.g., portrait/landscape)
//         PageFormat pf = job.pageDialog(job.defaultPage());
         PageFormat pf = job.validatePage(job.defaultPage());

         // Specify the Printable is an instance of
         // PrintListingPainter; also provide given PageFormat
         job.setPrintable(new PrintListingPainter(logName, app.getUserFont()), pf);

         // Print 1 copy
//         job.setCopies(1);

         // Print the job if the user didn't cancel printing
         try
         {
            app.startWait();
            job.print();
            app.stopWait();
         }
         catch (Exception ex)
         {
            JOptionPane.showMessageDialog(null, ex.getMessage(),
               "Error", JOptionPane.ERROR_MESSAGE);
         }
      }
   }

   // Exit
   void exitButton_actionPerformed(ActionEvent e)
   {
      try
      {
         // Close the session
         if (ls.ExecStr("kwi('" + id + "', close, _)") == 0 )
            JOptionPane.showMessageDialog(null, "kwi / close failed",
               "Error", JOptionPane.ERROR_MESSAGE);

         ls.Close();
         app.toggleRunMenu(false);
      }
      catch (Exception ex)
      {
//         displayKBError(ex, "'" + id + "'");
      }

      // Save the screen locations
      Rectangle r = this.getBounds();
      app.setConfigEntry("run_log_x", new Integer(r.x).toString());
      app.setConfigEntry("run_log_y", new Integer(r.y).toString());
      app.setConfigEntry("run_log_h", new Integer(r.height).toString());
      app.setConfigEntry("run_log_w", new Integer(r.width).toString());
      r = htmlPane.getBounds();
      app.setConfigEntry("run_out_x", new Integer(r.x).toString());
      app.setConfigEntry("run_out_y", new Integer(r.y).toString());
      app.setConfigEntry("run_out_h", new Integer(r.height).toString());
      app.setConfigEntry("run_out_w", new Integer(r.width).toString());

      htmlPane.dispose();
      // Following causes Error #-1 to print out
      dispose();
   }


   public boolean kbLoop(boolean stepping)
   {
      File log, output;
      long solveTerm, actionTerm, responseTerm, term;
      String responseFunctor, responseType, solutions, s;
      boolean more = true, oneAnswer;
      Vector aList = new Vector();

      setBusy(true);
      log = new File(logName);
      refreshLog(log);

      try
      {
         // Loop until there is no more
         while((solveTerm = ls.ExecStr("kwi('" + id + "', solve, _VAR_MORE)")) != 0)
         {
            // Check if we're done
            if (!ls.GetStrArg(solveTerm, 3).equals("more"))
               more = false;

            // Clear out the output text
            solutions = "";
            aList.clear();

            // Process all the actions
            do
            {
               // Update the status
               if ((term = ls.ExecStr("kwi('" + id + "', get_session, _VAR_IP)")) != 0)
               {
                  stateTextArea.setText("");
                  long statusList = ls.GetArg(term, 3);
                  Properties info = Utils.prologListToProperties(ls, statusList, 100000);
                  stateTextArea.append("Goals: " + info.getProperty("goals") + "\n");
                  long factList = ls.StrToTerm(info.getProperty("known"));
                  Vector facts = Utils.prologListToNameValueVector(ls, factList, 100000);
                  stateTextArea.append("Known:\n");
                  for (int i = 0 ; i < facts.size() ; i++)
                     stateTextArea.append("  " + facts.elementAt(i) + "\n");
               }

               // Update the log
               refreshLog(log);

               // Get the action
               if ((actionTerm = ls.ExecStr("kwi('" + id + "', get_action, _VAR_ACTION)")) == 0)
               {
                  JOptionPane.showMessageDialog(null, "kwi / get_action",
                     "Error", JOptionPane.ERROR_MESSAGE);
                     return false;
               }

               responseTerm = ls.GetArg(actionTerm, 3);
               responseFunctor = ls.GetFunctor(responseTerm);
               responseType = ls.GetStrArg(responseTerm, 1);

               // Display the HTML question page
               if (responseFunctor.equals("ask") && responseType.equals("html"))
               {
                  Properties question = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 3), 5000);
                  Vector qList = Utils.prologListToVector(ls, ls.GetArg(responseTerm, 2), 1000);
                  htmlPane.setContents(question.getProperty("html")/*, qList*/);
               }

               // Get a fact from the user
               if (responseFunctor.equals("ask") && responseType.equals("user"))
               {
                  String factName, prompt, questionType, defaultValue, answerType, delimiter;
                  int length;
                  Vector menu;
                  Properties tableMenu;
                  AskMenuDialog askMenu;
                  AskFieldDialog askField;
                  boolean invalidValue;

                  // The name of the fact we are seeking a value for
                  factName = ls.GetStrArg(responseTerm, 2);
                  Properties question = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 3), 5000);

                  // Get the prompt
                  prompt = question.getProperty("prompt");

                  // Get the question type
                  questionType = question.getProperty("question_type");

                  if (questionType.equals("fill_in_the_blank") || questionType.equals("fill_in_the_box"))
                  {
                     // Get the field length
                     length = new Integer((String)question.getProperty("length")).intValue();

                     // Get the default value
                     defaultValue = question.getProperty("default");

                     // Create the dialog box to ask the user
                     Point loc = app.getDesktopPane().getLocationOnScreen();
                     askField = new AskFieldDialog(app, "Question: " + factName, prompt, defaultValue, length, loc);

                     do
                     {
                        invalidValue = false;

                        // Can't use this during run because frame doesn't appear until the end
   //                     Dimension dlgSize = ask.getPreferredSize();
   //                     Dimension frmSize = app.getSize();
   //                     Point loc = app.getLocation();
   //                     ask.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);

                        askField.show();

                        // See if the user wants to stop
                        if (askField.getStopRun())
                        {
                           askField.dispose();
                           setBusy(false);
                           refreshLog(log);
                           return false;
                        }
                        else
                           askField.hide();

                        // Assert the new fact
                        answerType = question.getProperty("answer_type");

                        // Only text fields can be blank
                        if (!answerType.equals("text") && askField.getValue().length() == 0)
                        {
                           JOptionPane.showMessageDialog(null, "Value cannot be blank for fact " + factName,
                             "Error", JOptionPane.ERROR_MESSAGE);
                           invalidValue = true;
                        }

                        if (!invalidValue) askField.dispose();
                     }
                     while (invalidValue);

                     // Use strings for any field type, KW will check and convert
                     delimiter = "$";

                     if (ls.ExecStr("kwi('" + id + "', assert(fact('" + factName + "', " + delimiter + askField.getValue() + delimiter + ")), _)") == 0)
                        JOptionPane.showMessageDialog(null, "kwi / assert",
                          "Error", JOptionPane.ERROR_MESSAGE);
                  }
                  else /* Menu single and multiple choice */
                  {
                     if (question.getProperty("question_type").indexOf("menu_multiple_choices") < 0)
                        oneAnswer = true;
                     else
                        oneAnswer = false;

                     Point loc = app.getDesktopPane().getLocationOnScreen();
                     if (question.getProperty("question_type").indexOf("display_separate") < 0)
                     {
                        menu = Utils.prologListToVector(ls, ls.StrToTerm(question.getProperty("choices")), 5000);
                        askMenu = new AskMenuDialog(app, "Fact: " + factName, prompt, menu, oneAnswer, loc);
                     }
                     else
                     {
                        tableMenu = Utils.prologTextTableToProperties(ls, ls.StrToTerm(question.getProperty("rule-display_choices")));
                        menu = new Vector();
                        Enumeration e = tableMenu.propertyNames();
                        while (e.hasMoreElements()) menu.add(e.nextElement());
                        askMenu = new AskMenuDialog(app, "Fact: " + factName, prompt, menu, oneAnswer, loc);
                     }

                     // Ask the user
                     askMenu.show();

                     // See if the user wants to stop
                     if (askMenu.getStopRun())
                     {
                        askMenu.dispose();
                        setBusy(false);
                        refreshLog(log);
                        return false;
                     }
                     else
                        askMenu.dispose();

                     // Assert the new fact or facts (for multiple choices)
                     if (oneAnswer)
                        s = "kwi('" + id + "', assert(fact('" + factName + "', $" + askMenu.getSelectedValue() + "$)), _)";
                     else
                     {
                        Vector facts = askMenu.getSelectedValues();
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
                  // The name of the fact we are seeking a value for
                  Properties answer = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 2), 100000);
                  String answerName = answer.getProperty("goal");

                  // Add new solution onto existing ones
                  String newSolution = answer.getProperty("text");
                  if (newSolution != null)
                  {
                     aList.addElement(answerName);
                     if (solutions.length() == 0)
                        solutions = newSolution;
                     else
                        solutions = solutions + newSolution;
                  }
                  else
                     JOptionPane.showMessageDialog(app, "No text for solution: " + answerName,
                        "Error", JOptionPane.ERROR_MESSAGE);

               }

               // Display an external action
               if (responseFunctor.equals("tell") && responseType.equals("external"))
               {
                  Properties answer = Utils.prologListToProperties(ls, ls.GetArg(responseTerm, 2), 1000000);
                  String action = "Take Action: \n";
                  Enumeration e = answer.propertyNames();
                  while (e.hasMoreElements())
                  {
                     String element = (String)e.nextElement();
                     action = action + element + " = " + answer.getProperty(element) + "\n";
                  }
                  TellDialog tellUser = new TellDialog(app, "Tell / External", action);
                  Point loc = app.getDesktopPane().getLocationOnScreen();
                  tellUser.setLocation(loc.x, loc.y);
                  tellUser.setModal(true);
                  tellUser.show();

                  // See if the user wants to stop
                  if (tellUser.getStopRun())
                  {
                     tellUser.dispose();
                     setBusy(false);
                     refreshLog(log);
                     return false;
                  }
                  else
                     tellUser.dispose();
               }
            }
            while (!responseFunctor.equals("none"));

            // Put the output in a file for the back button to work
            if (solutions.length() > 0)
            {
               try
               {
                  htmlOutput = new File(app.tempDirectory + "kwtemp.html");
                  htmlFileWriter = new BufferedWriter(new FileWriter(htmlOutput));
                  htmlFileWriter.write(solutions);
                  htmlFileWriter.flush();
                  htmlFileWriter.close();
               }
               catch (IOException ex)
               {
                  System.out.println(ex);
               }

               // Display the solutions
               try
               {
                  htmlPane.setContents(solutions/*, aList*/);
               }
               catch (Exception ex)
               {
                  JOptionPane.showMessageDialog(app, "Invalid HTML: " + solutions + "\n" +
                     "(Try using text objects for output, or clear output_top/bottom/continue in knowledgebase main.)",
                     "Warning", JOptionPane.WARNING_MESSAGE);

               }

               htmlPane.repaint();

               String tellOut = "New output to tell the user is shown in the output window. The object(s) are:" + "\n";
               for (int i = 0 ; i < aList.size() ; i++)
                  tellOut = tellOut + aList.elementAt(i).toString() + "\n";

               TellDialog tellUser = new TellDialog(app, "Tell / User", tellOut);
               Point loc = app.getDesktopPane().getLocationOnScreen();
               tellUser.setLocation(loc.x, loc.y);
               tellUser.setModal(true);
               tellUser.show();

               // See if the user wants to stop
               if (tellUser.getStopRun())
               {
                  tellUser.dispose();
                  setBusy(false);
                  refreshLog(log);
                  return false;
               }
               else
                  tellUser.dispose();
            }

            // Bug out in single step mode
            if (stepping && more)
            {
               setBusy(false);
               break;
            }

            if (!more)
            {
               setBusy(false);
               runButton.setEnabled(false);
               stepButton.setEnabled(false);
               htmlOutput = new File(app.tempDirectory + "kwtemp.html");
               htmlOutput.deleteOnExit();
               break;
            }

         } // while solveTerm

      }
      catch (LSException e)
      {
         setBusy(false);
         refreshLog(log);
         displayKBError(e, "'" + id + "'");
         return false;
      }

      return true;
   }

   private void refreshLog(File log)
   {
      logTextArea.setText(null);
      try
      {
         logTextArea.read(new InputStreamReader(new FileInputStream(log), "UTF-16"), null);
         this.repaint();
      }
      catch (IOException e)
      {
         JOptionPane.showMessageDialog(null, "Unable to read log file " + logName,
            "Error", JOptionPane.ERROR_MESSAGE);
      }
   }

   public void displayKBError(LSException e, String id)
   {
      OpenFile openFile;
      String s;
      long term, errorList;

      try
      {
         // KBI errors return a detailed
         if (e.GetMsg().indexOf("kwi_error(") >= 0)
         {
            s = "kwi(" + id + ", get_error, _VAR_ERROR)";
            term = ls.ExecStr(s);

            // If we can't get error information, don't die
            if (term == 0)
            {
               JOptionPane.showMessageDialog(app, "Unable to call kwi / get_error for: " + e.GetMsg(),
                  "Logic Server Error", JOptionPane.ERROR_MESSAGE);
               return;
            }

            Properties info = Utils.prologListToProperties(ls, ls.GetArg(term, 3), 10000);
            Enumeration propNames = info.propertyNames();
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
            JOptionPane.showMessageDialog(app, msg,
               "Logicbase Execution Error", JOptionPane.ERROR_MESSAGE);
         }
         // Consult errors (outside the LBI) open editor and show location of syntax error
         else if (e.GetType() == LSException.READ /*read*/)
         {
/*            openFile = app.findOpenFile(e.GetReadFileName());
            if (openFile != null)
               openFile.getFrame().show();
            else
               app.openEditFile(new File(e.GetReadFileName()));
*/
            String lineno = new Integer(e.GetLineno()).toString();
            String msg = e.GetMsg() + "\n" + " in file " + e.GetReadFileName() +
               "\n" + " at line " + lineno + "\n" + e.GetReadBuffer();
            JOptionPane.showMessageDialog(app, msg,
               "Syntax Error", JOptionPane.ERROR_MESSAGE);

            // And make it all disappear
            exitButton_actionPerformed(null);
          }

         // Unrecognized Logic Server error
         else
            JOptionPane.showMessageDialog(app, e.GetMsg(),
               "Logic Server Error", JOptionPane.ERROR_MESSAGE);
      }
      catch (LSException e2)
      {
         System.out.println("Error catching error");
         e.printStackTrace();
      }
   }

//----------------------------------------------------------------------------//

   private void setBusy(boolean busy)
   {
      if (busy)
      {
         stepButton.setEnabled(false);
         runButton.setEnabled(false);
         exitButton.setEnabled(false);
         lastCursor = getCursor();
         setCursor(new Cursor(Cursor.WAIT_CURSOR));
      }
      else
      {
         stepButton.setEnabled(true);
         runButton.setEnabled(true);
         exitButton.setEnabled(true);
         setCursor(lastCursor);
      }
   }

//----------------------------------------------------------------------------//

   // Don't think this is called
   private class kbRunFrameListener extends InternalFrameAdapter
   {
      public void internalFrameClosed(InternalFrameEvent e)
      {
         try
         {
            ls.Close();
         }
         catch (Exception ex)
         {
            System.out.println(ex.toString());
         }
      }
   }
}
