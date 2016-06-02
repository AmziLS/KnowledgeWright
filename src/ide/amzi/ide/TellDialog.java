
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import com.borland.dbswing.ColumnLayout;
import java.awt.event.*;

public class TellDialog extends JDialog
{
   JPanel formPanel = new JPanel();
   JTextArea promptTextArea;
   JButton okButton = new JButton();
   JButton stopButton = new JButton();
   JPanel buttonPanel = new JPanel();
   JScrollPane promptScroller;
   ColumnLayout columnLayout = new ColumnLayout();
   App app;
   boolean stopRun = false;

   public TellDialog(App app, String title, String prompt)
   {
      super(app, title, true);
      this.app = app;
      try
      {
//         this.setTitle(title);
         promptTextArea = new JTextArea(prompt);
         jbInit();
         pack();
         Dimension ps = this.getPreferredSize();
         this.setSize(ps);
         setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);

         /* Add window listener because requestFocus only works if the
            field is visible!!! */
         addWindowListener(new WindowAdapter() {
            public void windowOpened(WindowEvent evt) {
                 okButton.requestFocus();}
                 });
      }
      catch(Exception ex)
      {
         ex.printStackTrace();
      }
   }

   void jbInit() throws Exception
   {
      promptTextArea.setLineWrap(true);
      promptTextArea.setWrapStyleWord(true);
      promptTextArea.setColumns(30);
      promptTextArea.setFont(app.getUserFont());
      if ((promptTextArea.getText().length()/40) > 20)
         promptTextArea.setRows(20);
      else
         promptTextArea.setRows((promptTextArea.getText().length()/40)+2);
      promptTextArea.setBorder(BorderFactory.createRaisedBevelBorder());
      promptTextArea.setEditable(false);
      promptTextArea.setCaretPosition(0);
      promptScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      promptScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      promptScroller.getViewport().add(promptTextArea, null);

      formPanel.setLayout(new BorderLayout());
      formPanel.add(promptScroller, BorderLayout.CENTER);
      formPanel.setBorder(BorderFactory.createRaisedBevelBorder());

      this.getRootPane().setDefaultButton(okButton);
      okButton.setText("OK");
      okButton.setMnemonic(KeyEvent.VK_ENTER);
      okButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            okButton_actionPerformed(e);
         }
      });
      stopButton.setText("Stop Run");
      stopButton.setMnemonic(KeyEvent.VK_S);
      stopButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            stopButton_actionPerformed(e);
         }
      });
      columnLayout.setHorizontalFill(true);
      buttonPanel.setLayout(columnLayout);
      buttonPanel.add(okButton, null);
      buttonPanel.add(stopButton, null);

//      getContentPane().setBorder(BorderFactory.createRaisedBevelBorder());
      getContentPane().add(formPanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.EAST);

      this.validate();
   }

   public boolean getStopRun()
   {
      return stopRun;
   }

   void okButton_actionPerformed(ActionEvent e)
   {
      stopRun = false;
      this.hide();
   }

   void stopButton_actionPerformed(ActionEvent e)
   {
      stopRun = true;
      this.hide();
   }

}

