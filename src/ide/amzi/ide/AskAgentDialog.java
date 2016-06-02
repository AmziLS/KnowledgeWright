// This snippet creates a new dialog box
// with buttons on the side.

//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package amzi.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import com.borland.dbswing.ColumnLayout;

public class AskAgentDialog extends JDialog
{
   JPanel dialogPanel = new JPanel();
   JPanel mainPanel = new JPanel();
   JButton okButton = new JButton();
   Border border1;
   Border border2;
   JPanel jPanel1 = new JPanel();
   JPanel jPanel2 = new JPanel();
   GridBagLayout gridBagLayout1 = new GridBagLayout();
   GridBagLayout gridBagLayout2 = new GridBagLayout();
   JLabel agentReturnedLabel = new JLabel();
   JScrollPane jScrollPane1 = new JScrollPane();
   JTextArea agentTextArea = new JTextArea();
   GridBagLayout gridBagLayout3 = new GridBagLayout();
   ColumnLayout columnLayout = new ColumnLayout();

  public AskAgentDialog(JFrame app, String text)
  {
    super(app, "", true);
    try
    {
      jbInit();
      agentTextArea.setText(text);
      this.setTitle("Agent Query");
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    pack();
  }

   private void jbInit() throws Exception
   {
      border1 = BorderFactory.createRaisedBevelBorder();
      border2 = BorderFactory.createEtchedBorder();
      mainPanel.setBorder(border1);
      mainPanel.setLayout(gridBagLayout3);
      okButton.setText("OK");
      okButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            okButton_actionPerformed(e);
         }
      });
      columnLayout.setHorizontalFill(true);
      jPanel2.setLayout(columnLayout);
      jPanel1.setLayout(gridBagLayout1);
      dialogPanel.setLayout(gridBagLayout2);
      agentReturnedLabel.setText("Agent returned the following:");
      agentTextArea.setLineWrap(true);
      agentTextArea.setWrapStyleWord(true);
      agentTextArea.setEditable(false);
      dialogPanel.add(mainPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 8), 0, 0));
      mainPanel.add(agentReturnedLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 0, 0), 173, 4));
      mainPanel.add(jScrollPane1, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 2, 4, 0), 0, 248));
      jScrollPane1.getViewport().add(agentTextArea, null);
      dialogPanel.add(jPanel2, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets(12, 0, 12, 8), 0, 0));
      jPanel2.add(okButton, null);
      getContentPane().add(dialogPanel);
  }


  // OK
  void okButton_actionPerformed(ActionEvent e)
  {
    dispose();
  }

}


