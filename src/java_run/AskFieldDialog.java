
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import com.borland.dbswing.ColumnLayout;
import java.awt.event.*;

public class AskFieldDialog extends JDialog
{
   JPanel formPanel = new JPanel();
   GridBagLayout gbl = new GridBagLayout();
   ColumnLayout formColumnLayout = new ColumnLayout();
   JTextArea promptTextArea;
   JButton okButton = new JButton();
   JButton stopButton = new JButton();
   JPanel buttonPanel = new JPanel();
   JTextField field;
   JScrollPane promptScroller;
   ColumnLayout columnLayout = new ColumnLayout();
   JFrame app;
   boolean stopRun = false;

   public AskFieldDialog(JFrame app, String title, String prompt, String defaultValue, int fieldLength)
   {
      super(app, title, true);
      this.app = app;
      try
      {
//         this.setTitle(title);
         promptTextArea = new JTextArea(prompt);
         field = new JTextField(defaultValue);
         if (fieldLength < 20)
            field.setColumns(20);
         else
            field.setColumns(fieldLength);
         jbInit();
         pack();
         Dimension ps = this.getPreferredSize();
         this.setSize(ps);
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
      promptTextArea.setBorder(BorderFactory.createRaisedBevelBorder());
      promptTextArea.setEditable(false);
      promptScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      promptScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      promptScroller.getViewport().add(promptTextArea, null);

      formColumnLayout.setHorizontalFill(true);
      formPanel.setLayout(formColumnLayout);
      formPanel.add(promptScroller);
      formPanel.add(field);
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

      getContentPane().add(formPanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.EAST);

      field.requestDefaultFocus();
      this.validate();
   }

   public String getValue()
   {
      return field.getText();
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

