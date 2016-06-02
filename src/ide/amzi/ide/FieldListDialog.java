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

public class FieldListDialog extends JDialog
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
   JLabel fieldLabel = new JLabel();
   JTextField field = new JTextField();
   JLabel listLabel = new JLabel();
   ColumnLayout columnLayout = new ColumnLayout();
   JList list;
   GridBagLayout gridBagLayout3 = new GridBagLayout();

  public FieldListDialog(App app, String title, String fieldPrompt, String listPrompt, Vector listValues, int selectedIndex)
  {
    super(app, title, true);
    try
    {
      fieldLabel.setText(fieldPrompt);
      listLabel.setText(listPrompt);
      list = new JList(listValues);
      list.setSelectedIndex(selectedIndex);
      jbInit();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    pack();
  }

  public FieldListDialog(App app, String title, String fieldPrompt, String listPrompt, Vector listValues, String selectedValue)
  {
    super(app, title, true);
    try
    {
      fieldLabel.setText(fieldPrompt);
      listLabel.setText(listPrompt);
      list = new JList(listValues);
      list.setSelectedValue((Object)selectedValue, true);
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
      border1 = BorderFactory.createRaisedBevelBorder();
      border2 = BorderFactory.createEtchedBorder();
      mainPanel.setBorder(border1);
      mainPanel.setLayout(gridBagLayout3);
      okButton.setText("OK");
      okButton.addActionListener(new FieldListDialog_okButton_actionAdapter(this));
      okButton.setEnabled(false);
      cancelButton.setText("Cancel");
      columnLayout.setHorizontalFill(true);
      buttonPanel.setLayout(columnLayout);
      jPanel1.setLayout(gridBagLayout1);
      cancelButton.addActionListener(new FieldListDialog_cancelButton_actionAdapter(this));
      this.addWindowListener(new FieldListDialog_this_windowAdapter(this));
      dialogPanel.setLayout(gridBagLayout2);
      field.setNextFocusableComponent(okButton);
      field.requestFocus();
      field.getDocument().addDocumentListener(new FieldListDialog_field_actionAdapter(this));
      list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      list.setNextFocusableComponent(okButton);
      list.addListSelectionListener(new FieldListDialog_list_actionAdapter(this));
      dialogPanel.add(mainPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
         ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 8), 0, 0));
      mainPanel.add(fieldLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
         ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(8, 6, 0, 0), 9, 2));
      mainPanel.add(field, new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
         ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(8, 0, 0, 5), 221, 0));
      JScrollPane listScrollPane = new JScrollPane(list);
      mainPanel.add(listScrollPane, new GridBagConstraints(1, 1, 1, 1, 1.0, 1.0
         ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(12, 0, 9, 5), 225, 131));
      mainPanel.add(listLabel, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
         ,GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, new Insets(12, 6, 122, 0), 15, 1));
      dialogPanel.add(buttonPanel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
         ,GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets(4, 0, 6, 8), 0, 0));
      buttonPanel.add(okButton, null);
      buttonPanel.add(cancelButton, null);
      getContentPane().add(dialogPanel);
  }

   public String getFieldText()
   {
      return field.getText();
   }

   public String getListItem()
   {
      return list.getSelectedValue().toString();
   }

   public void setListItem(String type)
   {
      list.setSelectedValue((Object)type, true);
   }

   public void setFieldText(String name)
   {
      field.setText(name);
   }

   // OK
   void okButton_actionPerformed(ActionEvent e)
   {
      if (field.getText().length() != 0 && !list.isSelectionEmpty())
         dispose();
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Cancel
   void cancelButton_actionPerformed(ActionEvent e)
   {
      field.setText("");
      dispose();
   }

   void this_windowClosing(WindowEvent e)
   {
      field.setText("");
      dispose();
   }

   void enableButtons()
   {
      if (field.getText().length() > 0 && list.getSelectedIndex() >= 0)
         okButton.setEnabled(true);
      else
         okButton.setEnabled(false);
   }
}

class FieldListDialog_field_actionAdapter implements DocumentListener{
  FieldListDialog adaptee;

  FieldListDialog_field_actionAdapter(FieldListDialog adaptee) {
    this.adaptee = adaptee;
  }

   public void insertUpdate(DocumentEvent e)
   {
      adaptee.enableButtons();
   }

   public void removeUpdate(DocumentEvent e)
   {
      adaptee.enableButtons();
   }

   public void changedUpdate(DocumentEvent e)
   {
      adaptee.enableButtons();
   }
}

class FieldListDialog_list_actionAdapter implements ListSelectionListener{
  FieldListDialog adaptee;

  FieldListDialog_list_actionAdapter(FieldListDialog adaptee) {
    this.adaptee = adaptee;
  }

   public void valueChanged(ListSelectionEvent e)
   {
      adaptee.enableButtons();
   }
}
class FieldListDialog_okButton_actionAdapter implements ActionListener{
  FieldListDialog adaptee;

  FieldListDialog_okButton_actionAdapter(FieldListDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.okButton_actionPerformed(e);
  }
}

class FieldListDialog_cancelButton_actionAdapter implements ActionListener{
  FieldListDialog adaptee;

  FieldListDialog_cancelButton_actionAdapter(FieldListDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.cancelButton_actionPerformed(e);
  }
}

class FieldListDialog_this_windowAdapter extends WindowAdapter {
  FieldListDialog adaptee;

  FieldListDialog_this_windowAdapter(FieldListDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void windowClosing(WindowEvent e) {
    adaptee.this_windowClosing(e);
  }
}

