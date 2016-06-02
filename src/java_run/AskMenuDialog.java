
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

public class AskMenuDialog extends JDialog
{
   JTextArea promptTextArea;
   ColumnLayout formColumnLayout = new ColumnLayout();
   JButton okButton = new JButton();
   JButton stopButton = new JButton();
   JPanel buttonPanel = new JPanel();
   JPanel formPanel = new JPanel();
   JList menuList;
   JScrollPane menuScroller = new JScrollPane();
   JScrollPane promptScroller = new JScrollPane();
   JLabel selectLabel = new JLabel();
   GridBagLayout gbl = new GridBagLayout();
   ColumnLayout columnLayout = new ColumnLayout();
   boolean oneAnswer;
   boolean stopRun;
   JFrame app;

   public AskMenuDialog(JFrame app, String title, String prompt, Vector menu, boolean oneAnswer)
   {
      super(app, title, true);
      try
      {
//         this.setTitle(title);
         this.oneAnswer = oneAnswer;
         this.app = app;
         promptTextArea = new JTextArea(prompt);
//         promptTextArea.setColumns(30);
         menuList = new JList(menu);
/*         if (menu.size() > 20)
            menuList.setVisibleRowCount(20);
         else
            menuList.setVisibleRowCount(menu.size());
*/
         if (menu.size() < 20)
            menuList.setVisibleRowCount(menu.size());
         if (oneAnswer)
         {
            selectLabel.setText("Select one of the following:");
            menuList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
         }
         else
         {
            selectLabel.setText("Select any number of the following:");
            menuList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
         }
         jbInit();
         menuList.setSelectedIndex(0);
         menuList.requestFocus();
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
      this.getContentPane().setLayout(new BorderLayout());

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

      promptTextArea.setLineWrap(true);
      promptTextArea.setWrapStyleWord(true);
      promptTextArea.setBorder(BorderFactory.createRaisedBevelBorder());
      promptTextArea.setEditable(false);
      promptTextArea.setRequestFocusEnabled(false);
      promptScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      promptScroller.getViewport().add(promptTextArea, null);
      menuScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      menuScroller.getViewport().add(menuList, null);

      formColumnLayout.setHorizontalFill(true);
      formColumnLayout.setVgap(1);
      formPanel.setLayout(formColumnLayout);
      formPanel.add(promptScroller);
      formPanel.add(selectLabel);
      formPanel.add(menuScroller);
      formPanel.setBorder(BorderFactory.createRaisedBevelBorder());

      this.getContentPane().add(formPanel, BorderLayout.CENTER);
      this.getContentPane().add(buttonPanel, BorderLayout.EAST);
   }

   public String getSelectedValue()
   {
      if (menuList.isSelectionEmpty())
         return null;
      else
         return menuList.getSelectedValue().toString();
   }

   public Vector getSelectedValues()
   {
      Object[] facts = menuList.getSelectedValues();
      Vector v = new Vector();
      for (int i = 0 ; i < facts.length ; i++)
         v.addElement((String)facts[i]);
      return v;
   }

   public boolean getStopRun()
   {
      return stopRun;
   }

   void okButton_actionPerformed(ActionEvent e)
   {
      stopRun = false;
      if (!menuList.isSelectionEmpty())
         hide();
   }

   void stopButton_actionPerformed(ActionEvent e)
   {
      stopRun = true;
      menuList.clearSelection();
      hide();
   }

}

