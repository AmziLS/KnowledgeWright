// This snippet creates a new dialog box
// with buttons on the side.

//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package  amzi.ide;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import com.borland.dbswing.ColumnLayout;

public class KBNewObjectDialog extends JDialog implements /*KeyListener,*/
   DocumentListener, ListSelectionListener
{
  JPanel dialogPanel = new JPanel();
  JPanel mainPanel = new JPanel();
  JButton okButton = new JButton();
  JButton cancelButton = new JButton();
  Border border1;
  Border border2;
  JPanel jPanel1 = new JPanel();
  JPanel buttonPanel = new JPanel();
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  GridBagLayout gridBagLayout2 = new GridBagLayout();
   JLabel nameLabel = new JLabel();
   JTextField nameField = new JTextField();
   JLabel typeLabel = new JLabel();
   ColumnLayout columnLayout = new ColumnLayout();
   JList typeList;
   GridBagLayout gridBagLayout3 = new GridBagLayout();

  public KBNewObjectDialog(App app, Vector types)
  {
    super(app, "New Object", true);
    try
    {
      typeList = new JList(types);
      jbInit();
       pack();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

  private void jbInit() throws Exception
  {
      border1 = BorderFactory.createRaisedBevelBorder();
      border2 = BorderFactory.createEtchedBorder();
      mainPanel.setBorder(border1);
      mainPanel.setLayout(gridBagLayout3);

      this.getRootPane().setDefaultButton(okButton);
      okButton.setText("OK");
      okButton.setMnemonic(KeyEvent.VK_ENTER);
      okButton.addActionListener(new java.awt.event.ActionListener(){
         public void actionPerformed(ActionEvent e)
         {
            okButton_actionPerformed(e);
         }
      });
      okButton.setEnabled(false);

      cancelButton.setText("Cancel");
      cancelButton.setMnemonic(KeyEvent.VK_ESCAPE);
      columnLayout.setHorizontalFill(true);
      buttonPanel.setLayout(columnLayout);
      jPanel1.setLayout(gridBagLayout1);
      cancelButton.addActionListener(new java.awt.event.ActionListener(){
         public void actionPerformed(ActionEvent e)
         {
            cancelButton_actionPerformed(e);
         }
      });
      dialogPanel.setLayout(gridBagLayout2);
      nameLabel.setText("Name:");
      nameField.setNextFocusableComponent(okButton);
      nameField.requestFocus();
      nameField.getDocument().addDocumentListener(this);
//      nameField.addKeyListener(this);
      typeLabel.setText("Type:");
      typeList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      typeList.setSelectedIndex(-1);
      typeList.setNextFocusableComponent(okButton);
      typeList.addListSelectionListener(this);
      dialogPanel.add(mainPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
         ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 8), 0, 0));
      mainPanel.add(nameLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
         ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(8, 6, 0, 0), 9, 2));
      mainPanel.add(nameField, new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
         ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(8, 0, 0, 5), 221, 0));
      JScrollPane listScrollPane = new JScrollPane(typeList);
      mainPanel.add(listScrollPane, new GridBagConstraints(1, 1, 1, 1, 1.0, 1.0
         ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(12, 0, 9, 5), 225, 131));
      mainPanel.add(typeLabel, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
         ,GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, new Insets(12, 6, 122, 0), 15, 1));
      dialogPanel.add(buttonPanel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
         ,GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets(4, 0, 6, 8), 0, 0));
      buttonPanel.add(okButton, null);
      buttonPanel.add(cancelButton, null);
      getContentPane().add(dialogPanel);
  }

   public String getName()
   {
      String s = nameField.getText();
      return nameField.getText();
   }

   public String getType()
   {
      String s = typeList.getSelectedValue().toString();
      return typeList.getSelectedValue().toString();
   }

   public void setType(String type)
   {
      typeList.setSelectedValue((Object)type, true);
   }

   public void setName(String name)
   {
      nameField.setText(name);
   }

   // OK
   void okButton_actionPerformed(ActionEvent e)
   {
      if (nameField.getText().length() != 0 && !typeList.isSelectionEmpty())
         dispose();
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Cancel
   void cancelButton_actionPerformed(ActionEvent e)
   {
      nameField.setText("");
      dispose();
   }

   void enableButtons()
   {
      if (nameField.getText().length() > 0 && typeList.getSelectedIndex() >= 0)
      {
         nameField.setNextFocusableComponent(okButton);
         typeList.setNextFocusableComponent(okButton);
         okButton.setEnabled(true);
      }
      else
         okButton.setEnabled(false);
   }

//----------------------------------------------------------------------------//

   // KeyListener
/*
   public void keyTyped(KeyEvent e)
   {
   }
   public void keyPressed(KeyEvent e)
   {
   }
   public void keyReleased(KeyEvent e)
   {
      if (e.getKeyCode() == KeyEvent.VK_ENTER)
         okButton_actionPerformed(null);
   }
*/
//----------------------------------------------------------------------------//

   // DocumentListener

   public void insertUpdate(DocumentEvent e)
   {
      enableButtons();
   }

   public void removeUpdate(DocumentEvent e)
   {
      enableButtons();
   }

   public void changedUpdate(DocumentEvent e)
   {
      enableButtons();
   }

//----------------------------------------------------------------------------//

   // ListSelectionListener

   public void valueChanged(ListSelectionEvent e)
   {
      enableButtons();
      if (nameField.getText().length() > 0) okButton.requestFocus();
   }


//----------------------------------------------------------------------------//

   // WindowListener

   protected void processWindowEvent(WindowEvent e)
   {
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
         nameField.setText("");
         dispose();
      }
   }

}


