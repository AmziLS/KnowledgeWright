
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
   App app;

   public AskMenuDialog(App app, String title, String prompt, Vector menu, boolean oneAnswer, Point loc)
   {
      super(app, title, true /*modal*/);
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
         pack();
         Dimension ps = this.getPreferredSize();
         setSize(ps);
         setLocation(loc.x, loc.y);
//         setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);

         // Mouse listener does it all for double-click
         menuList.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
               if (evt.getClickCount() == 2)
               {
                  stopRun = false;
                  if (!menuList.isSelectionEmpty()) hide();
               }
            }
            });

         menuList.setSelectedIndex(0);
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

      promptTextArea.setFont(app.getUserFont());
      promptTextArea.setLineWrap(true);
      promptTextArea.setWrapStyleWord(true);
      promptTextArea.setBorder(BorderFactory.createRaisedBevelBorder());
      promptTextArea.setEditable(false);
      promptTextArea.setRequestFocusEnabled(false);
      promptScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      promptScroller.getViewport().add(promptTextArea, null);
      menuScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      menuList.setFont(app.getUserFont());
      menuScroller.getViewport().add(menuList, null);

/*      formPanel.setLayout(gbl);
      formPanel.add(promptScroller, new GridBagConstraints(0,0, 1,1, 1.0,1.0,
         GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, new Insets(2,2,2,2), 0,0));
      formPanel.add(selectLabel, new GridBagConstraints(0,GridBagConstraints.RELATIVE, 1,GridBagConstraints.RELATIVE, 0.0,0.0,
         GridBagConstraints.SOUTHWEST, GridBagConstraints.NONE, new Insets(2,2,2,2), 0,0));
      formPanel.add(menuScroller, new GridBagConstraints(0,GridBagConstraints.RELATIVE, 1,GridBagConstraints.REMAINDER, 1.0,1.0,
         GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, new Insets(2,2,2,2), 0,0));
      formPanel.setBorder(BorderFactory.createRaisedBevelBorder());
*/

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

//----------------------------------------------------------------------------//

   // WindowListener

   protected void processWindowEvent(WindowEvent e)
   {
      // Request focus only works if the field is visible!
      if (e.getID() == WindowEvent.WINDOW_OPENED)
         menuList.requestFocus();
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
         stopButton_actionPerformed(null);
   }

}

