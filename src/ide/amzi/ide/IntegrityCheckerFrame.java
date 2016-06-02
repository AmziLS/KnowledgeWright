/**
 * Title:
 * Description:
 * Copyright:    Copyright (c)2000-2001 Amzi! inc.
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class IntegrityCheckerFrame extends JPanel
{
   BorderLayout mainLayout = new BorderLayout();
   JPanel buttonPanel = new JPanel();
   FlowLayout buttonLayout = new FlowLayout(FlowLayout.CENTER);
   JButton refreshButton = new JButton();
   JButton closeButton = new JButton();
   JScrollPane problemScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
   JTextArea problemsTextArea = new JTextArea();
   final App app;
   KB kb;

   public IntegrityCheckerFrame(App app, KB kb, boolean check)
   {
      super();
      this.app = app;
      this.kb = kb;
      try
      {
         jbInit();
         if (check) recheck();
      }
      catch(Exception ex) {
         ex.printStackTrace();
      }
   }

   private void jbInit() throws Exception
   {
      this.setLayout(mainLayout);

      // Recheck Button
      refreshButton.setText("Recheck");
      refreshButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            refreshButton_actionPerformed(e);
         }
      });
      buttonPanel.add(refreshButton);
      this.add(buttonPanel, BorderLayout.EAST);

      // Problems
      problemsTextArea.setFont(app.getUserFont());
      problemsTextArea.setEditable(false);
      problemScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      problemScroller.getViewport().add(problemsTextArea, null);
      this.add(problemScroller, BorderLayout.CENTER);
   }

   public void recheck()
   {
//      setCursor(new Cursor(Cursor.WAIT_CURSOR));

      // The following only displays on startup
      problemsTextArea.setText("Checking knowledgebase for problems...please wait");
      refreshButton.setEnabled(false);
      this.repaint();

      String results = null;
      try
      {
         results = kb.checkIntegrity();
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }

      displayResults(results);

//      setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
   }

   private void displayResults(String s)
   {
      problemsTextArea.setText(s);
      problemsTextArea.setCaretPosition(0);
      refreshButton.setEnabled(true);
      this.repaint();
   }

   public void refreshButton_actionPerformed(ActionEvent e)
   {
      recheck();
   }

   public void setTextFont(Font f)
   {
      problemsTextArea.setFont(f);
      this.repaint();
   }

}