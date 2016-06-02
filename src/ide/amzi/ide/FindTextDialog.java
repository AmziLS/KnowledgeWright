
//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package  amzi.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.text.*;
import com.borland.dbswing.ColumnLayout;
import javax.swing.event.*;

public class FindTextDialog extends JDialog implements KeyListener
{
   JPanel findPanel = new JPanel();
   JPanel buttonPanel = new JPanel();
   JButton findButton = new JButton();
   JButton cancelButton = new JButton();
   Border bevelBorder;
   BorderLayout mainLayout = new BorderLayout();
   ColumnLayout buttonColumnLayout = new ColumnLayout();
   ColumnLayout findColumnLayout = new ColumnLayout();
   JLabel findTextLabel = new JLabel();
   JTextField findText = new JTextField();
   JCheckBox matchCase = new JCheckBox();

   App app;

   public FindTextDialog(App app)
   {
      super(app, "Find" /*title*/, true /*modal*/);
      this.app = app;
      try
      {
         jbInit();
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
      pack();
   }

   private void jbInit() throws Exception
   {
      this.getContentPane().setLayout(mainLayout);

      bevelBorder = BorderFactory.createRaisedBevelBorder();
      findPanel.setBorder(bevelBorder);
      findTextLabel.setText("Find text:");
      findText.setMinimumSize(new Dimension(50, 21));
      findText.setPreferredSize(new Dimension(200, 21));
      matchCase.setText("Match Case");
      findPanel.setLayout(findColumnLayout);
      findPanel.add(findText);
      findPanel.add(matchCase);

      this.getRootPane().setDefaultButton(findButton);
      findButton.setText("Find");
      findButton.setMnemonic(KeyEvent.VK_ENTER);
      findButton.addActionListener(new java.awt.event.ActionListener(){
         public void actionPerformed(ActionEvent e)
         {
            findButton_actionPerformed(e);
         }
      });

      cancelButton.setText("Cancel");
      cancelButton.setMnemonic(KeyEvent.VK_ESCAPE);
      cancelButton.addActionListener(new java.awt.event.ActionListener(){
         public void actionPerformed(ActionEvent e)
         {
            cancelButton_actionPerformed(e);
         }
      });
      buttonColumnLayout.setHorizontalFill(true);
      buttonPanel.setLayout(buttonColumnLayout);
      buttonPanel.add(findButton, null);
      buttonPanel.add(cancelButton, null);

      this.getContentPane().add(findPanel, BorderLayout.CENTER);
      this.getContentPane().add(buttonPanel, BorderLayout.EAST);

      addKeyListener(this);
   }

   public String getText()
   {
      return findText.getText();
   }

   public boolean getMatchcase()
   {
      return matchCase.isSelected();
   }

   // Find
   void findButton_actionPerformed(ActionEvent e)
   {
      if (findText.getText().length() == 0)
         Toolkit.getDefaultToolkit().beep();
      else
         dispose();
   }

   // Cancel
   void cancelButton_actionPerformed(ActionEvent e)
   {
      findText.setText("");
      dispose();
   }

//----------------------------------------------------------------------------//

   // WindowListener

   protected void processWindowEvent(WindowEvent e)
   {
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
         findText.setText("");
         dispose();
      }
   }

//----------------------------------------------------------------------------//

   // KeyListener

   public void keyTyped(KeyEvent e)
   {
   }
   public void keyPressed(KeyEvent e)
   {
   }
   public void keyReleased(KeyEvent e)
   {
      if (e.getKeyCode() == KeyEvent.VK_ENTER)
         findButton_actionPerformed(null);
   }


}

