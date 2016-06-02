
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

public class AskFieldDialog extends JDialog
{
   JPanel formPanel = new JPanel();
   GridBagLayout gbl = new GridBagLayout();
   ColumnLayout formColumnLayout = new ColumnLayout();
   JTextArea promptTextArea;
   JButton okButton = new JButton();
   JButton stopButton = new JButton();
   JPanel buttonPanel = new JPanel();
   static JTextField field;
   JScrollPane promptScroller;
   ColumnLayout columnLayout = new ColumnLayout();
   App app;
   boolean stopRun = false;

   public AskFieldDialog(App app, String title, String prompt, String defaultValue, int fieldLength, Point loc)
   {
      super(app, title, true /*modal*/);
      this.app = app;
      try
      {
         promptTextArea = new JTextArea(prompt);
         field = new JTextField(defaultValue);
         if (fieldLength < 20)
            field.setColumns(20);
         else
            field.setColumns(fieldLength);
         jbInit();
         pack();
         Dimension ps = this.getPreferredSize();
         setSize(ps);
         setLocation(loc.x, loc.y);
//         setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
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
/*      if ((promptTextArea.getText().length()/40) > 30)
         promptTextArea.setRows(30);
      else
         promptTextArea.setRows((promptTextArea.getText().length()/30)+2);
*/      promptTextArea.setBorder(BorderFactory.createRaisedBevelBorder());
      promptTextArea.setEditable(false);
      promptScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      promptScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      promptScroller.getViewport().add(promptTextArea, null);

      field.setFont(app.getUserFont());

/*      formPanel.setLayout(gbl);
      formPanel.add(promptScroller, new GridBagConstraints(0,0, 1,3, 1.0,1.0,
         GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(2,2,2,2), 0,0));
      formPanel.add(field, new GridBagConstraints(0,3, 1,1, 1.0,1.0,
         GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2,2,2,2), 0,0));
      formPanel.setBorder(BorderFactory.createRaisedBevelBorder());
*/

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
      buttonPanel.add(okButton, null);
      buttonPanel.add(stopButton, null);

//      getContentPane().setBorder(BorderFactory.createRaisedBevelBorder());
      getContentPane().add(formPanel, BorderLayout.CENTER);
      getContentPane().add(buttonPanel, BorderLayout.EAST);

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

//----------------------------------------------------------------------------//

   // WindowListener

   protected void processWindowEvent(WindowEvent e)
   {
      // Request focus only works if the field is visible!
      if (e.getID() == WindowEvent.WINDOW_OPENED)
         field.requestFocus();
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
         stopButton_actionPerformed(null);
   }

}

