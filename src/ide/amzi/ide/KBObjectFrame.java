
//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package  amzi.ide;

import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import com.borland.dbswing.ColumnLayout;

public class KBObjectFrame extends JInternalFrame
   implements DocumentListener, TableModelListener, ListDataListener,
      KeyListener, TableColumnModelListener, MouseListener, InternalFrameListener
{
   private BorderLayout borderLayout1 = new BorderLayout();
   ResourceBundle res = ResourceBundle.getBundle("amzi.ide.resources.IDE");
   private JPanel buttonPanel = new JPanel();
   private JScrollPane editScrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
   private JButton helpButton = new JButton();
   private JButton saveButton = new JButton();
   private JButton saveCloseButton = new JButton();
   private JButton closeButton = new JButton();
   private FlowLayout flowLayout1 = new FlowLayout();
   private JPanel editPanel = new JPanel();
   private GridBagLayout gridBagLayout1 = new GridBagLayout();
   private JTable table = null;
   private JMenuBar menuBar = new JMenuBar();
   private JMenu editMenu = new JMenu();
   private JMenu menuObject = new JMenu();
   private JMenuItem menuObjectSave = new JMenuItem();
   private JMenuItem menuObjectSaveClose = new JMenuItem();
   private JMenuItem menuObjectClose = new JMenuItem();
   private JMenuItem menuObjectPrint = new JMenuItem();
   private JMenu menuEdit = new JMenu();
   private JMenuItem menuEditUndo = new JMenuItem();
   private JMenuItem menuEditRedo = new JMenuItem();
   private JMenuItem menuEditCut = new JMenuItem();
   private JMenuItem menuEditCopy = new JMenuItem();
   private JMenuItem menuEditPaste = new JMenuItem();
   private JMenuItem menuSelectAll = new JMenuItem();
   private JMenu menuHelp = new JMenu();
   private JMenuItem menuHelpObject = new JMenuItem();
   private JPopupMenu popup = new JPopupMenu();
   private JMenuItem popupOpenObject = new JMenuItem();
   private JMenuItem popupMakeLink = new JMenuItem();
   private JMenuItem popupEditCut = new JMenuItem();
   private JMenuItem popupEditCopy = new JMenuItem();
   private JMenuItem popupEditPaste = new JMenuItem();
   private JMenuItem popupInsertName = new JMenuItem();

   // Listener for the edits on the current document
   protected UndoableEditListener undoHandler = new UndoHandler();
   // UndoManager that we add edits to
   protected UndoManager undo = new UndoManager();

   private App app;
   private KBObject object;
   private KBTreeFrame tree;
   private boolean dirty;
   private JTextComponent lastTextComponent = null;
   private boolean addedTableUndo = false;
   private String docsDirectory;
   private SavedSettings settings;

   private static final String upString = "Move up";
   private static final String downString = "Move down";

   public KBObjectFrame(App app, KBTreeFrame tree, KBObject object, int locX, int locY,
      int bottom, SavedSettings settings, String docsDirectory)
   {
      super(object.getType() + ": " + object.getName(), true, true, true, true);
      this.app = app;
      this.tree = tree;
      this.object = object;
      this.settings = settings;
      this.docsDirectory = docsDirectory;

      setDirty(false);
      try
      {
         jbInit();
         buildForm(locX, locY, bottom);
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
           "Internal Error", JOptionPane.ERROR_MESSAGE);
         ex.printStackTrace();
      }
   }

   private void jbInit() throws Exception
   {
      this.setVisible(true);
      this.setClosable(true);
      this.setIconifiable(true);
      ImageIcon icon = object.getIcon();
      this.setFrameIcon(icon);
      this.setMaximizable(true);
      this.setResizable(true);
      this.getContentPane().setLayout(borderLayout1);
      this.addInternalFrameListener(this);

      // Edit Menu
      menuEdit.setText(res.getString("menuEdit.Text"));
      menuEdit.setMnemonic(KeyEvent.VK_E);

      menuEditUndo.setText(res.getString("menuUndo.Text"));
      menuEditUndo.setMnemonic(KeyEvent.VK_U);
      menuEditUndo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, ActionEvent.CTRL_MASK));
      menuEditUndo.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditUndo_actionPerformed(e);
         }
      });
	   menuEditUndo.setHorizontalTextPosition(JButton.RIGHT);

      menuEditRedo.setText(res.getString("menuRedo.Text"));
      menuEditRedo.setMnemonic(KeyEvent.VK_R);
      menuEditRedo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, ActionEvent.CTRL_MASK+ActionEvent.SHIFT_MASK));
      menuEditRedo.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditRedo_actionPerformed(e);
         }
      });
	   menuEditRedo.setHorizontalTextPosition(JButton.RIGHT);
      menuEditRedo.setEnabled(false);

      menuEditCut.setToolTipText(res.getString("menuCut.ToolTipText"));
      menuEditCut.setText(res.getString("menuCut.Text"));
      menuEditCut.setMnemonic(KeyEvent.VK_T);
      menuEditCut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK));
      menuEditCut.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditCut_actionPerformed(e);
         }
      });
	   menuEditCut.setHorizontalTextPosition(JButton.RIGHT);
	   menuEditCut.setIcon(new ImageIcon(getClass().getResource("resources/cut.gif")));

      menuEditCopy.setText(res.getString("menuCopy.Text"));
      menuEditCopy.setMnemonic(KeyEvent.VK_C);
      menuEditCopy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
      menuEditCopy.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditCopy_actionPerformed(e);
         }
      });
	   menuEditCopy.setHorizontalTextPosition(JButton.RIGHT);
	   menuEditCopy.setIcon(new ImageIcon(getClass().getResource("resources/copy.gif")));

      menuEditPaste.setText(res.getString("menuPaste.Text"));
      menuEditPaste.setMnemonic(KeyEvent.VK_P);
      menuEditPaste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
      menuEditPaste.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditPaste_actionPerformed(e);
         }
      });
	   menuEditPaste.setHorizontalTextPosition(JButton.RIGHT);
	   menuEditPaste.setIcon(new ImageIcon(getClass().getResource("resources/paste.gif")));

      menuSelectAll.setText(res.getString("menuSelectAll.Text"));
      menuSelectAll.setMnemonic(KeyEvent.VK_A);
      menuSelectAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));
      menuSelectAll.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuSelectAll_actionPerformed(e);
         }
      });

      menuObject.setText("Object");
      menuObject.setMnemonic(KeyEvent.VK_O);

      menuObjectSave.setText("Save");
      menuObjectSave.setMnemonic(KeyEvent.VK_S);
      menuObjectSave.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
      menuObjectSave.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectSave_actionPerformed(e);
         }
      });

      menuObjectSaveClose.setActionCommand("SaveClose");
      menuObjectSaveClose.setMnemonic(KeyEvent.VK_A);
      menuObjectSaveClose.setText("Save & Close");
      menuObjectSaveClose.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectSaveClose_actionPerformed(e);
         }
      });

      menuObjectClose.setText("Close");
      menuObjectClose.setMnemonic(KeyEvent.VK_C);
      menuObjectClose.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0));
      menuObjectClose.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectClose_actionPerformed(e);
         }
      });

      menuObjectPrint.setText("Print");
      menuObjectPrint.setMnemonic(KeyEvent.VK_P);
      menuObjectPrint.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectPrint_actionPerformed(e);
         }
      });

      menuHelp.setText("Help");
      menuHelp.setMnemonic(KeyEvent.VK_H);

      menuHelpObject.setText("Object");
      menuHelpObject.setMnemonic(KeyEvent.VK_O);
      // The keyboard listener actually implements this. This just shows the F1 on the menu.
      menuHelpObject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
      menuHelpObject.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpObject_actionPerformed(e);
         }
      });

      menuObject.add(menuObjectSave);
      menuObject.add(menuObjectSaveClose);
      menuObject.addSeparator();
      menuObject.add(menuObjectPrint);
      menuObject.addSeparator();
      menuObject.add(menuObjectClose);
      menuEdit.add(menuEditUndo);
      menuEdit.add(menuEditRedo);
      menuEdit.addSeparator();
      menuEdit.add(menuEditCut);
      menuEdit.add(menuEditCopy);
      menuEdit.add(menuEditPaste);
      menuEdit.addSeparator();
      menuEdit.add(menuSelectAll);
      menuHelp.add(menuHelpObject);
      menuBar.add(menuObject);
      menuBar.add(menuEdit);
      this.setJMenuBar(menuBar);

      // Pop up menu
      popupInsertName.setText("Insert object name at cursor");
      popupInsertName.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            popupInsertName_actionPerformed(e);
         }
      });
      popupOpenObject.setText("Open object name at cursor");
      popupOpenObject.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            popupOpenObject_actionPerformed(e);
         }
      });
      popupMakeLink.setText("Make text into object link");
      popupMakeLink.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            popupMakeLink_actionPerformed(e);
         }
      });
      popupEditCut.setText(res.getString("menuCut.Text"));
      popupEditCut.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditCut_actionPerformed(e);
         }
      });
      popupEditCopy.setText(res.getString("menuCopy.Text"));
      popupEditCopy.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditCopy_actionPerformed(e);
         }
      });
      popupEditPaste.setText(res.getString("menuPaste.Text"));
      popupEditPaste.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuEditPaste_actionPerformed(e);
         }
      });
      popup.add(popupOpenObject);
      popup.add(popupMakeLink);
      popup.addSeparator();
      popup.add(popupEditCut);
      popup.add(popupEditCopy);
      popup.add(popupEditPaste);

      // Buttons
      helpButton.setText("Help");
      helpButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            helpButton_actionPerformed(e);
         }
      });

      saveButton.setText("Save");
      saveButton.setMnemonic(KeyEvent.VK_S);
      saveButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            saveButton_actionPerformed(e);
         }
      });
      saveCloseButton.setText(res.getString("buttonSaveClose.Text"));
      saveCloseButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            saveCloseButton_actionPerformed(e);
         }
      });
      closeButton.setText(res.getString("buttonClose.Text"));
      closeButton.setMnemonic(KeyEvent.VK_ESCAPE);
      closeButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            closeButton_actionPerformed(e);
         }
      });
      buttonPanel.add(helpButton, null);
      buttonPanel.add(saveButton, null);
      buttonPanel.add(saveCloseButton, null);
      buttonPanel.add(closeButton, null);
      buttonPanel.setLayout(flowLayout1);
      editPanel.setLayout(gridBagLayout1);
//      this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
//      this.getContentPane().add(editPanel, BorderLayout.CENTER);
      editScrollPane.getViewport().add(editPanel, null);
      editScrollPane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      this.getContentPane().add(editScrollPane, BorderLayout.CENTER);
   }

   // Dynamically build a form for editing the object
   private boolean buildForm(int locX, int locY, int bottom)
   {
      Vector slotNames;
      int width;

      try
      {
         slotNames = object.getSlotNames();
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
         return false;
      }

      addSlots(slotNames, editPanel);
      this.pack();

      Dimension d = this.getPreferredSize();
      // Add some extra room for the table scroll bar--Java bug
      int w = d.width+20;
      int h = d.height;
      Dimension bigd = app.getDesktopPane().getSize();
      if (locX+w > bigd.width) w = bigd.width-(locX+1);
//      if (locY+h > bigd.height) h = bigd.height-(locY+1);
      if (locY+h > bottom) h = bottom-(locY+1);
      this.setBounds(locX, locY, w, h);

      // Put the help menu last
      menuBar.add(menuHelp);

      return true;
   }

   // Add all the slots to a panel with a GridBagLayout
   private void addSlots(Vector slotNames, JPanel slotPanel)
   {
      String s, spaces = "                                                                                ";
      int i, type;
      JLabel label;
      Dimension d;
      int height, row, total_height, max_object_width, max_label_width;
      Vector menus = new Vector();
      Font font = app.getUserFont();

//      slotPanel.addKeyListener(new KeyAdapter() {
//                              public void keyReleased(KeyEvent evt) {
//                              Toolkit.getDefaultToolkit().beep();
//                              }});

      total_height = 0;
      max_label_width = 0;
      max_object_width = 0;
      row = 0;
      for (i = 0 ; i < slotNames.size() ; i++)
      {
         // Get the slot name
         s = slotNames.elementAt(i).toString();

         // Calculate the height of the slot
         height = 1;
/*         try
         {
            if (object.getSlotDisplayType(s) == KBObject.TEXT_BOX ||
                object.getSlotDisplayType(s) == KBObject.TABLE ||
                object.getSlotDisplayType(s) == KBObject.TEXT_TABLE)
               height = 3;
            else if (object.getSlotDisplayType(s) == KBObject.LIST_OBJECT)
               height = 2;
            else
               height = 1;
            if (object.getSlotValueType(s) == KBObject.RULES)
               height = height * 2;
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
               ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            height = 1;
         }
*/
         // Add the slot name
         label = new JLabel(s);
         label.setFont(font);

         slotPanel.add(label, new GridBagConstraints(0,row,
            1,height, 0.0,0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE,
            new Insets(2,4,2,4), 0,0));

         // Add the slot value
         try
         {
            type = object.getSlotDisplayType(s);
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
               ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            type = KBObject.UNKNOWN;
         }
         switch (type)
         {
            case KBObject.MENU:
               JComboBox menu;
               try
               {
                  menu = new JComboBox(object.getMenuValues(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  menu = new JComboBox();
               }
               try
               {
                  menu.setSelectedItem((Object)object.getTextSlot(s));
                  menu.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  menu.setSelectedItem("");
               }
               menu.setEditable(false);
               menu.setFont(font);

               slotPanel.add(menu, new GridBagConstraints(1,row,
                  1,height, 1.0,0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL,
                  new Insets(2,2,2,2), 0,0));

               // Make a list of menus so we can set the visible/invisible slots after
               // all the slots have been added
               menus.addElement(menu);

               // Add the listener last to avoid inadvertant dirties
               menu.addItemListener(new MenuItemListener(slotPanel, menu, object, s));
               menu.addKeyListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) menu.requestDefaultFocus();
               break;

            case KBObject.ID:
               JLabel idfield;
               try
               {
                  String fieldStr = object.getTextSlot(s);
                  idfield = new JLabel(fieldStr + spaces.substring(1, 80-fieldStr.length()));
                  idfield.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  idfield = new JLabel("");
               }

               idfield.setFont(font);

               slotPanel.add(idfield, new GridBagConstraints(1,row,
                  1,height, 1.0,0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL,
                  new Insets(2,2,2,2), 0,0));
               break;

            case KBObject.TEXT_LINE:
            case KBObject.FILE_PATH:
               JTextField field;
               try
               {
                  String fieldStr = object.getTextSlot(s);
                  field = new JTextField(fieldStr);
                  if (fieldStr.length() < 50) field.setColumns(fieldStr.length());
                  else field.setColumns(50);
                  field.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  field = new JTextField("");
               }
               field.setEditable(true);
               field.setFont(font);

               slotPanel.add(field, new GridBagConstraints(1,row,
                  1,height, 1.0,0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL,
                  new Insets(2,2,2,2), 0,0));

               // Add the listener last (to avoid inadvertent dirties)
               field.getDocument().addDocumentListener(this);
               field.addKeyListener(this);

     	         // Add this as a listener for undoable edits.
               Document fieldDoc = field.getDocument();
     	         fieldDoc.addUndoableEditListener(undoHandler);

               // Add a focus listener for cut, copy, paste, select
               field.addFocusListener(new FocusListener() {
                  public void focusGained(FocusEvent e) {
                     lastTextComponent = (JTextComponent)e.getComponent(); }
                  public void focusLost(FocusEvent e) {
                     lastTextComponent = null; } });

               // Add a mouse listener for the popup menu
               field.addMouseListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) field.requestDefaultFocus();
               break;

            case KBObject.TEXT_BOX:
               JScrollPane textScrollerb = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
               textScrollerb.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
               slotPanel.add(textScrollerb, new GridBagConstraints(1,row,
                  1,height, 1.0,1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                  new Insets(2,2,2,2), 0,0));
               JTextArea textb = new JTextArea();
               textb.setEditable(true);
               textb.setFont(font);
               textb.setWrapStyleWord(true);
               textb.setLineWrap(true);
               try
               {
/*                  if (object.getSlotValueType(s) == KBObject.RULES)
                  {
                     textb.setText("");
                     Vector rules = object.getRuleSlot(s);
                     for (int j = 0 ; j < rules.size() ; j++)
                        textb.append((String)rules.elementAt(j));
                  }
                  else
*/
                  textb.setText(object.getTextSlot(s));
                  textb.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  textb.setText("");
               }

               // Give the user some place to type
               if (textb.getText().length() == 0)
                  textb.setRows(3);
               else
               {
                  String txt = textb.getText();
                  int lineCount = 0;
                  for (int ccnt = 0 ; ccnt < txt.length() ; ccnt++)
                     if (txt.substring(ccnt, ccnt).equals("\n"))
                        lineCount++;
                  if ((lineCount + txt.length()/33) < 3)
                     textb.setRows(3);
                  else
                     textb.setRows(lineCount + txt.length()/33);
               }

               // Put the text box in a scroller
               textScrollerb.getViewport().add(textb, null);

               // Make sure the cursor is at the beginning
               textb.setCaretPosition(0);

               // Add the listener last (to avoid inadvertent dirties)
               textb.getDocument().addDocumentListener(this);
               textb.addKeyListener(this);

     	         // Add this as a listener for undoable edits.
               Document textbDoc = textb.getDocument();
     	         textbDoc.addUndoableEditListener(undoHandler);

               // Add a focus listener for cut, copy, paste, select
               textb.addFocusListener(new FocusListener() {
                  public void focusGained(FocusEvent e) {
                     lastTextComponent = (JTextComponent)e.getComponent();
//                     lastTextComponent.setCaretPosition(0);
//                     Caret c = lastTextComponent.getCaret();
//                     c.setVisible(true);
                  }
                  public void focusLost(FocusEvent e) {
                     lastTextComponent = null; } });

               // Add a mouse listener for the popup menu
               textb.addMouseListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) textb.requestDefaultFocus();
               break;

            case KBObject.LIST_OBJECT:
//            case KBObject.LIST_OF_TERMS:
               Vector listData = new Vector();

               // First create the list
               DefaultListModel listModel = new DefaultListModel();
               JList listbox = new JList(listModel);
               listbox.setFont(font);
               listbox.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
               listbox.clearSelection();
               int maxDataLen = 0;
               try
               {
                  listData = object.getListSlot(s);
                  for (int j = 0 ; j < listData.size() ; j ++)
                  {
                     listModel.add(j, listData.get(j));
                     if (listData.get(j).toString().length() > maxDataLen)
                        maxDataLen = listData.get(j).toString().length();
                  }
                  if (listData.size() < 3)
                  {
                     listbox.setVisibleRowCount(3);
                     listModel.ensureCapacity(3);
                  }
                  else if (listData.size() > 12)
                     listbox.setVisibleRowCount(listData.size()/2);

                  listbox.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  listbox.setListData(new Vector(0));
               }

               // Then the buttons to modify the list
               if (maxDataLen < 15) maxDataLen = 15;
               if (maxDataLen > 30) maxDataLen = 30;
               JTextField addText = new JTextField("", maxDataLen);
               addText.setFont(font);
               JButton addButton = new JButton("Add");
               JButton chgButton = new JButton("Change");
               JButton delButton = new JButton("Delete");
               JButton upButton = new JButton(new ImageIcon(getClass().getResource("resources/up.gif")));
               upButton.setMargin(new Insets(0,0,0,0));
               upButton.setActionCommand(upString);
               JButton downButton = new JButton(new ImageIcon(getClass().getResource("resources/down.gif")));
               downButton.setMargin(new Insets(0,0,0,0));
               downButton.setActionCommand(downString);

               // Turn off the buttons if the list is empty
               if (listData.size() == 0)
               {
                  upButton.setEnabled(false);
                  downButton.setEnabled(false);
                  chgButton.setEnabled(false);
                  delButton.setEnabled(false);
               }

               // Add action listeners for all the buttons and the list
               addText.addActionListener(new AddButtonListener(addButton, chgButton, delButton, upButton, downButton, addText, listbox));
               addButton.addActionListener(new AddButtonListener(addButton, chgButton, delButton, upButton, downButton, addText, listbox));
               chgButton.addActionListener(new ChangeButtonListener(addButton, chgButton, delButton, upButton, downButton, addText, listbox));
               delButton.addActionListener(new DelButtonListener(delButton, upButton, downButton, listbox));
               upButton.addActionListener(new UpDownListener(upButton, downButton, listbox));
               downButton.addActionListener(new UpDownListener(upButton, downButton, listbox));
               listbox.addListSelectionListener(new ListListener(addButton, chgButton, delButton, upButton, downButton, addText, listbox));
               listbox.addKeyListener(this);

               JPanel upDownPanel = new JPanel(new GridLayout(2, 1));
               upDownPanel.add(upButton);
               upDownPanel.add(downButton);
               JPanel listButtons = new JPanel(new FlowLayout(FlowLayout.LEFT));
               listButtons.add(addButton);
               listButtons.add(chgButton);
               listButtons.add(delButton);
               listButtons.add(upDownPanel);
               ColumnLayout listLayout = new ColumnLayout();
               JPanel listControls = new JPanel(listLayout);
               listControls.add(addText);
               listControls.add(listButtons);

               // Put the list in a scroll panel
               JScrollPane listScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
               listScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
               listScroller.getViewport().add(listbox, null);

               // Put the controls and the list in another panel and add it
               JPanel listPanel = new JPanel(new BorderLayout());

               // The order in which these are added is depended upon in saveSlots!
               listPanel.add(listScroller, BorderLayout.CENTER);
               listPanel.add(listControls, BorderLayout.EAST);
               slotPanel.add(listPanel, new GridBagConstraints(1,row,
                  1,height, 1.0,1.0, GridBagConstraints.EAST, GridBagConstraints.BOTH,
                  new Insets(2,2,2,2), 0,0));

               // Add the listener last (to avoid inadvertent dirties)
               listModel.addListDataListener(this);

     	         // Add this as a listener for undoable edits.
               Document addTextDoc = addText.getDocument();
     	         addTextDoc.addUndoableEditListener(undoHandler);

               // Add a focus listener for cut, copy, paste, select
               addText.addFocusListener(new FocusListener() {
                  public void focusGained(FocusEvent e) {
                     lastTextComponent = (JTextComponent)e.getComponent(); }
                  public void focusLost(FocusEvent e) {
                     lastTextComponent = null; } });

               // Add a mouse listener for the popup menu
               addText.addMouseListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) addText.requestDefaultFocus();
               break;

            case KBObject.NAME_LIST:
               Vector listDatan = new Vector();
               Vector addData = null;

               // First create the list
               DefaultListModel listModeln = new DefaultListModel();
               JList listboxn = new JList(listModeln);
               listboxn.setFont(font);
               listboxn.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
               listboxn.clearSelection();
               int maxDataLenn = 0;
               try
               {
                  // Get the list of possible values
                  addData = object.getSlotListChoices(s);

                  // Get the list of actual values
                  listDatan = object.getListSlot(s);
                  for (int j = 0 ; j < listDatan.size() ; j ++)
                  {
                     listModeln.add(j, listDatan.get(j));
                     if (listDatan.get(j).toString().length() > maxDataLenn)
                        maxDataLenn = listDatan.get(j).toString().length();
                  }
                  if (listDatan.size() < 3)
                  {
                     listboxn.setVisibleRowCount(3);
                     listModeln.ensureCapacity(3);
                  }
                  else if (listDatan.size() > 12)
                     listboxn.setVisibleRowCount(listDatan.size()/2);

                  listboxn.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  listboxn.setListData(new Vector(0));
               }

               // Create the combo box
               addData.insertElementAt("(Type here or select)", 0);
               JComboBox addTextn = new JComboBox(addData);
               addTextn.setFont(font);
               addTextn.setEditable(true);

               // Then the buttons to modify the list
               if (maxDataLenn < 15) maxDataLenn = 15;
               if (maxDataLenn > 30) maxDataLenn = 30;
               JButton addButtonn = new JButton("Add");
               JButton chgButtonn = new JButton("Change");
               JButton delButtonn = new JButton("Delete");
               JButton upButtonn = new JButton(new ImageIcon(getClass().getResource("resources/up.gif")));
               upButtonn.setMargin(new Insets(0,0,0,0));
               upButtonn.setActionCommand(upString);
               JButton downButtonn = new JButton(new ImageIcon(getClass().getResource("resources/down.gif")));
               downButtonn.setMargin(new Insets(0,0,0,0));
               downButtonn.setActionCommand(downString);

               // Turn off the buttons if the list is empty
               if (listDatan.size() == 0)
               {
                  upButtonn.setEnabled(false);
                  downButtonn.setEnabled(false);
                  chgButtonn.setEnabled(false);
                  delButtonn.setEnabled(false);
               }

               // Add action listeners for all the buttons and the list
               addTextn.addActionListener(new AddButtonComboListener(addButtonn, chgButtonn, delButtonn, upButtonn, downButtonn, addTextn, listboxn));
               addButtonn.addActionListener(new AddButtonComboListener(addButtonn, chgButtonn, delButtonn, upButtonn, downButtonn, addTextn, listboxn));
               chgButtonn.addActionListener(new ChangeButtonComboListener(addButtonn, chgButtonn, delButtonn, upButtonn, downButtonn, addTextn, listboxn));
               delButtonn.addActionListener(new DelButtonListener(delButtonn, upButtonn, downButtonn, listboxn));
               upButtonn.addActionListener(new UpDownListener(upButtonn, downButtonn, listboxn));
               downButtonn.addActionListener(new UpDownListener(upButtonn, downButtonn, listboxn));
               listboxn.addListSelectionListener(new ListComboListener(addButtonn, chgButtonn, delButtonn, upButtonn, downButtonn, addTextn, listboxn));
               listboxn.addKeyListener(this);

               JPanel upDownPaneln = new JPanel(new GridLayout(2, 1));
               upDownPaneln.add(upButtonn);
               upDownPaneln.add(downButtonn);
               JPanel listButtonsn = new JPanel(new FlowLayout(FlowLayout.LEFT));
               listButtonsn.add(addButtonn);
               listButtonsn.add(chgButtonn);
               listButtonsn.add(delButtonn);
               listButtonsn.add(upDownPaneln);
               ColumnLayout listLayoutn = new ColumnLayout();
               JPanel listControlsn = new JPanel(listLayoutn);
               listControlsn.add(addTextn);
               listControlsn.add(listButtonsn);

               // Put the list in a scroll panel
               JScrollPane listScrollern = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
               listScrollern.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
               listScrollern.getViewport().add(listboxn, null);

               // Put the controls and the list in another panel and add it
               JPanel listPaneln = new JPanel(new BorderLayout());

               // The order in which these are added is depended upon in saveSlots!
               listPaneln.add(listScrollern, BorderLayout.CENTER);
               listPaneln.add(listControlsn, BorderLayout.EAST);
               slotPanel.add(listPaneln, new GridBagConstraints(1,row,
                  1,height, 1.0,1.0, GridBagConstraints.EAST, GridBagConstraints.BOTH,
                  new Insets(2,2,2,2), 0,0));

               // Add the listener last (to avoid inadvertent dirties)
               listModeln.addListDataListener(this);

     	         // Add this as a listener for undoable edits.
//               Document addTextDocn = addText.getDocument();
//     	         addTextDocn.addUndoableEditListener(undoHandler);

               // Add a focus listener for cut, copy, paste, select
               addTextn.addFocusListener(new FocusListener() {
                  public void focusGained(FocusEvent e) {
                     lastTextComponent = (JTextComponent)e.getComponent(); }
                  public void focusLost(FocusEvent e) {
                     lastTextComponent = null; } });

               // Add a mouse listener for the popup menu
               addTextn.addMouseListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) addTextn.requestDefaultFocus();
               break;

            case KBObject.TABLE:
               Vector rows, cols;
               try
               {
                  rows = object.getTableRows(s);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  rows = null;
               }
               try
               {
                  cols = object.getTableColumnNames(s);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  cols = null;
               }
               table = new JTable();
               table.setModel(new DefaultTableModel(rows, cols));
               JScrollPane tableScroller = new JScrollPane(table);
               tableScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
               tableScroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
               tableScroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
               try
               {
                  table.setToolTipText(object.getSlotHelp(s));
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               }

               slotPanel.add(tableScroller, new GridBagConstraints(1,row,
//               slotPanel.add(table, new GridBagConstraints(1,row,
                  1,height, 1.0,1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                  new Insets(0,0,0,0), 0,0));
               table.setColumnSelectionAllowed(false);
               table.setRowSelectionAllowed(false);
               table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
               table.setCellSelectionEnabled(false);
               table.sizeColumnsToFit(-1);
               JTextField tableTextField = new JTextField();
               tableTextField.setFont(font);
               DefaultCellEditor tableCellEditor = new DefaultCellEditor(tableTextField);
               tableCellEditor.setClickCountToStart(1);
               table.setDefaultEditor(Object.class, tableCellEditor);
               addTableMenu(false, false);

               // Add the listener last (to avoid inadvertent dirties)
               table.getModel().addTableModelListener(this);
               table.addKeyListener(this);

     	         // Add this as a listener for undoable edits.
               Document tableTextFieldDoc = tableTextField.getDocument();
               tableTextFieldDoc.addUndoableEditListener(undoHandler);

               // Add a mouse listener for the popup menu
               tableTextField.addMouseListener(this);

               // Add a selection listener for cut, copy, paste, select
               table.getSelectionModel().addListSelectionListener(new TableListener(table));

               // If it's the first editable field, get the focus
               if (row == 2) table.requestDefaultFocus();
               break;

            case KBObject.TEXT_TABLE:
               Vector rowsf, colsf;
//               int slotValueType;
               TextBoxCellEditor textBoxCellEditor;
               TextBoxCellRenderer tableCellRenderer;

               // Add a menu for table functions
               addTableMenu(true, true);

               try
               {
                  rowsf = object.getTableRows(s);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  rowsf = null;
               }
               try
               {
                  colsf = object.getTableColumnNames(s);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  colsf = null;
               }
               table = new JTable();
               table.setModel(new DefaultTableModel(rowsf, colsf));
               JScrollPane tableScrollerf = new JScrollPane(table);
               tableScrollerf.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
               tableScrollerf.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
               tableScrollerf.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
//               tableScrollerf.setSize(table.getPreferredSize());
               try
               {
                  table.setToolTipText(object.getSlotHelp(s));

                  // Make the language column narrower if this is not for rules
/*                  slotValueType = object.getSlotValueType(s);
                  if (slotValueType != KBObject.RULES)
                  {
                     JLabel dummy = new JLabel();
                     int colWidth = dummy.getFontMetrics(dummy.getFont()).charsWidth("language".toCharArray(), 0, "language".length());
                     table.getColumnModel().getColumn(0).setPreferredWidth(colWidth);
                     Dimension tableDim = table.getPreferredScrollableViewportSize();
                     table.getColumnModel().getColumn(1).setPreferredWidth(tableDim.width - colWidth);
                  }
*/               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               }

               // Must have scrollpane for column headers!
               slotPanel.add(tableScrollerf, new GridBagConstraints(1,row,
//               slotPanel.add(table, new GridBagConstraints(1,row,
                  1,height, 1.0,1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                  new Insets(0,0,0,0), 0,0));

               // Only allow one cell to be selected
               table.setColumnSelectionAllowed(false);
               table.setRowSelectionAllowed(false);
               table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
               table.setCellSelectionEnabled(false);

               // Don't allow columns to be moved
               table.getTableHeader().setReorderingAllowed(false);

               // Fit the columns in the available space
               table.sizeColumnsToFit(-1);

               // Setup renderer and editor. Rules are 5 rows high by default.
               JTextArea tableTextArea = new JTextArea();
               tableTextArea.setFont(font);
               table.setRowHeight(tableTextArea.getFontMetrics(tableTextArea.getFont()).getHeight()*5);
               textBoxCellEditor = new TextBoxCellEditor(tableTextArea, font);   // Pass same textArea for undo listener
               table.setDefaultEditor(Object.class, textBoxCellEditor);
               tableCellRenderer = new TextBoxCellRenderer(font);
               table.setDefaultRenderer(Object.class, tableCellRenderer);

               // Fix scroll bars
               Dimension tsize = table.getPreferredSize();
               table.setSize(tsize);
               table.setPreferredScrollableViewportSize(tsize);

     	         // Add this as a listener for undoable edits.
               Document tableTextAreaDoc = tableTextArea.getDocument();
               tableTextAreaDoc.addUndoableEditListener(undoHandler);

               // Add the listener last (to avoid inadvertent dirties)
               table.getModel().addTableModelListener(this);
               table.addKeyListener(this);

               // Add a mouse listener for the popup menu
               tableTextArea.addMouseListener(this);

               // Add a selection listener for cut, copy, paste, select
               table.getSelectionModel().addListSelectionListener(new TableListener(table));

               table.getColumnModel().addColumnModelListener(this);

               // If it's the first editable field, get the focus
               if (row == 2) table.requestDefaultFocus();
               break;

            case KBObject.UNKNOWN:
               break;
         }

         row = row + height;
      }

      // Touch all the menus so fields that are supposed to be invisible become so
      for (int j = 0 ; j < menus.size() ; j++)
      {
         int idx = ((JComboBox)menus.elementAt(j)).getSelectedIndex();
         if (idx == 0)
            ((JComboBox)menus.elementAt(j)).setSelectedIndex(1);
         else
            ((JComboBox)menus.elementAt(j)).setSelectedIndex(0);
         ((JComboBox)menus.elementAt(j)).setSelectedIndex(idx);
      }

      // And reset the dirty flag
      setDirty(false);

      return;
   }

   private void addTableMenu(boolean fixedColumns, boolean bigCells)
   {
      JMenu tableMenuRow = new JMenu();
      JMenuItem tableMenuRowAdd = new JMenuItem();
      JMenuItem tableMenuRowInsert = new JMenuItem();
      JMenuItem tableMenuRowDelete = new JMenuItem();
      JMenuItem tableMenuRowExpandAll = new JMenuItem();
      JMenu tableMenuColumn = new JMenu();
      JMenuItem tableMenuColumnAdd = new JMenuItem();
      JMenuItem tableMenuColumnRename = new JMenuItem();
      JMenuItem tableMenuColumnDelete = new JMenuItem();

      // Row Menu
      tableMenuRow.setText(res.getString("menuRow.Text"));
      tableMenuRow.setMnemonic(KeyEvent.VK_R);

      tableMenuRowAdd.setText(res.getString("menuAdd.Text"));
      tableMenuRowAdd.setMnemonic(KeyEvent.VK_A);
      tableMenuRowAdd.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, ActionEvent.CTRL_MASK));
      tableMenuRowAdd.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            tableMenuRowAdd_actionPerformed(e);
         }
      });

      tableMenuRowInsert.setText("Insert Above");
      tableMenuRowInsert.setMnemonic(KeyEvent.VK_I);
      tableMenuRowInsert.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            tableMenuRowInsert_actionPerformed(e);
         }
      });

      tableMenuRowDelete.setText(res.getString("menuDelete.Text"));
      tableMenuRowDelete.setMnemonic(KeyEvent.VK_D);
      tableMenuRowDelete.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, ActionEvent.CTRL_MASK));
      tableMenuRowDelete.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            tableMenuRowDelete_actionPerformed(e);
         }
      });

      tableMenuRowExpandAll.setText("Expand All");
      tableMenuRowExpandAll.setMnemonic(KeyEvent.VK_E);
      tableMenuRowExpandAll.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            tableMenuRowExpandAll_actionPerformed(e);
         }
      });

      menuBar.add(tableMenuRow);
      tableMenuRow.add(tableMenuRowAdd);
      tableMenuRow.add(tableMenuRowInsert);
      tableMenuRow.add(tableMenuRowDelete);
      if (bigCells)
      {
         tableMenuRow.addSeparator();
         tableMenuRow.add(tableMenuRowExpandAll);
      }

      if (!fixedColumns)
      {
         // Column Menu
         tableMenuColumn.setText(res.getString("menuColumn.Text"));
         tableMenuColumn.setMnemonic(KeyEvent.VK_C);

         tableMenuColumnAdd.setText(res.getString("menuAdd.Text"));
         tableMenuColumnAdd.setMnemonic(KeyEvent.VK_A);
         tableMenuColumnAdd.addActionListener(new java.awt.event.ActionListener()
         {
            public void actionPerformed(ActionEvent e)
            {
               tableMenuColumnAdd_actionPerformed(e);
            }
         });

         tableMenuColumnRename.setText(res.getString("menuRename.Text"));
         tableMenuColumnRename.setMnemonic(KeyEvent.VK_R);
         tableMenuColumnRename.addActionListener(new java.awt.event.ActionListener()
         {
            public void actionPerformed(ActionEvent e)
            {
               tableMenuColumnRename_actionPerformed(e);
            }
         });

         tableMenuColumnDelete.setText(res.getString("menuDelete.Text"));
         tableMenuColumnDelete.setMnemonic(KeyEvent.VK_D);
         tableMenuColumnDelete.addActionListener(new java.awt.event.ActionListener()
         {
            public void actionPerformed(ActionEvent e)
            {
               tableMenuColumnDelete_actionPerformed(e);
            }
         });

         menuBar.add(tableMenuColumn);
         tableMenuColumn.add(tableMenuColumnAdd);
         tableMenuColumn.add(tableMenuColumnRename);
         tableMenuColumn.add(tableMenuColumnDelete);
      }
   }

   private boolean saveSlots(JPanel slotPanel)
   {
      Vector v;
      int type;
      boolean success = true;

      // Kludge--Stupid Java tosses the value if the user doesn't select
      // another cell or press <enter>
      if (table != null)
      {
         if (table.isEditing())
            ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
      }

      for (int i = 0 ; i < slotPanel.getComponentCount() ; i += 2)
      {
         // Get the name from the label
         JLabel label = (JLabel)slotPanel.getComponent(i);
         String s = label.getText();

         // Get the value from the following component
         try
         {
            type = object.getSlotDisplayType(s);
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
               ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            type = KBObject.UNKNOWN;
         }

         switch (type)
         {
            case KBObject.MENU:
               JComboBox menu = (JComboBox)slotPanel.getComponent(i+1);
               try
               {
                  object.setSlot(s, menu.getSelectedItem().toString());
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               break;

            case KBObject.TEXT_LINE:
            case KBObject.FILE_PATH:
               JTextField field = (JTextField)slotPanel.getComponent(i+1);
               String fieldText = field.getText();
               if (type == KBObject.FILE_PATH) fieldText = Utils.doubleSlashes(fieldText);
               try
               {
                  object.setSlot(s, fieldText);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               field.getDocument().removeDocumentListener(this);
               break;

            case KBObject.TEXT_BOX:
               JScrollPane textScroller2 = (JScrollPane)slotPanel.getComponent(i+1);
               JTextArea text2 = (JTextArea)textScroller2.getViewport().getComponent(0);
               try
               {
                  if (object.getSlotValueType(s) == KBObject.RULES)
                  {
                     char lineSep[] = "\n".toCharArray();
                     object.setSlot(s, "[" + text2.getText().replace(lineSep[0], ' ') + "]");
                  }
                  else
                     object.setSlot(s, text2.getText());
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               text2.getDocument().removeDocumentListener(this);
               break;

            case KBObject.LIST_OBJECT:
//            case KBObject.LIST_OF_TERMS:
            case KBObject.NAME_LIST:
               JPanel listPanel = (JPanel)slotPanel.getComponent(i+1);
               // Get the listbox scroller (added first when the form was created)
               JScrollPane listScroller = (JScrollPane)listPanel.getComponent(0);
               JList list = (JList)listScroller.getViewport().getComponent(0);
               ListModel listModel = list.getModel();
               Vector listData = new Vector(((DefaultListModel)listModel).size());
               for (int j = 0 ; j < ((DefaultListModel)listModel).size() ; j++)
                  listData.add(j, ((DefaultListModel)listModel).get(j));
               try
               {
                  object.setSlot(s, listData);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               listModel.removeListDataListener(this);
               break;

            case KBObject.TABLE:
//               JTable table = (JTable)slotPanel.getComponent(i+1);
               JScrollPane tableScroller = (JScrollPane)slotPanel.getComponent(i+1);
               JTable table = (JTable)tableScroller.getViewport().getComponent(0);
               v = new Vector();

               // Get the column names first
               Vector names = getTableColumnNames(table);
               v.addElement(names);

               // Then add on the table values
               for (int r = 0 ; r < table.getRowCount() ; r++)
               {
                  Vector row = new Vector();
                  for (int c = 0 ; c < table.getColumnCount() ; c++)
                  {
                     Object o = table.getValueAt(r, c);
                     if (o == null) row.addElement("");
                     else row.addElement(o.toString());
                  }
                  v.addElement(row);
               }

               try
               {
                  object.setSlot(s, v);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               table.getModel().removeTableModelListener(this);
               break;

            case KBObject.TEXT_TABLE:
//               JTable tablef = (JTable)slotPanel.getComponent(i+1);
               JScrollPane tableScrollerf = (JScrollPane)slotPanel.getComponent(i+1);
               JTable tablef = (JTable)tableScrollerf.getViewport().getComponent(0);
               v = new Vector();

               // Get the column names first
               Vector namesf = getTableColumnNames(tablef);
               v.addElement(namesf);

               // Then add on the table values
               for (int r = 0 ; r < tablef.getRowCount() ; r++)
               {
                  Vector row = new Vector();
                  for (int c = 0 ; c < tablef.getColumnCount() ; c++)
                  {
                     Object o = tablef.getValueAt(r, c);
                     if (o == null) row.addElement("");
                     else row.addElement(o.toString());
                  }
                  v.addElement(row);
               }

               try
               {
                  object.setSlot(s, v);
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  success = false;
               }
               tablef.getModel().removeTableModelListener(this);
               break;

            case KBObject.ID:
               break;

            case KBObject.UNKNOWN:
               break;
         }
      }
      if (success) setDirty(false);
      return success;
   }

   private void setSlotVisible(JPanel slotPanel, String slot, boolean visible)
   {
      Vector v;
      int type;

      for (int i = 0 ; i < slotPanel.getComponentCount() ; i += 2)
      {
         // Get the name from the label
         JLabel label = (JLabel)slotPanel.getComponent(i);
         String s = label.getText();

         // If it matches, make the label and the component to its right in/visible
         if (s.equals(slot))
         {
            label.setVisible(visible);
            slotPanel.getComponent(i+1).setVisible(visible);
         }
      }
   }

   private Vector getTableColumnNames(JTable table)
   {
      Vector names = new Vector();
      for (int ci = 0 ; ci < table.getColumnCount() ; ci ++)
         names.addElement(table.getColumnName(ci));

      return names;
   }

//----------------------------------------------------------------------------//

   // Object | Save
   void menuObjectSave_actionPerformed(ActionEvent e)
   {
      saveSlots(editPanel);
   }

   // Object | Save & Close
   void menuObjectSaveClose_actionPerformed(ActionEvent e)
   {
      if (saveSlots(editPanel))
         closeButton_actionPerformed(null);
   }

   // Object | Close
   void menuObjectClose_actionPerformed(ActionEvent e)
   {
      closeButton_actionPerformed(null);
   }

   // Object | Print
   void menuObjectPrint_actionPerformed(ActionEvent e)
   {
      Properties prnProp = new Properties();
      PrintJob prnJob = Toolkit.getDefaultToolkit().getPrintJob(app, "Print Object", prnProp);
      if (prnJob != null)
      {
         Graphics prnGr = prnJob.getGraphics();

         //Print in the center of the page
         Dimension od = editPanel.getSize(); //Object size
         Dimension pd = prnJob.getPageDimension(); //Page size

         prnGr.translate((pd.width-od.width)/2, (pd.height-od.height)/2);
         editPanel.printAll(prnGr);

         prnGr.dispose(); //Prints here
         prnJob.end(); //Release printer
      }
   }

   // Edit | Undo
   private void menuEditUndo_actionPerformed(ActionEvent e)
   {
      try
      {
         undo.undo();
         toggleUndoRedo();
      }
      catch (CannotUndoException ex)
      {
         Toolkit.getDefaultToolkit().beep();
      }
   }

   // Edit | Redo
   private void menuEditRedo_actionPerformed(ActionEvent e)
   {
      try
      {
         undo.redo();
         toggleUndoRedo();
      }
      catch (CannotRedoException ex)
      {
         Toolkit.getDefaultToolkit().beep();
 	   }
   }

   private void toggleUndoRedo()
   {
      if(undo.canUndo())
         menuEditUndo.setEnabled(true);
      else
         menuEditUndo.setEnabled(false);
      if(undo.canRedo())
         menuEditRedo.setEnabled(true);
      else
         menuEditRedo.setEnabled(false);
   }

   // Edit | Cut
   private void menuEditCut_actionPerformed(ActionEvent e)
   {
      if (lastTextComponent != null)
      {
//         String s = lastTextComponent.getSelectedText();
         lastTextComponent.cut();
      }
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Edit | Copy
   private void menuEditCopy_actionPerformed(ActionEvent e)
   {
      if (lastTextComponent != null)
      {
//         String s = lastTextComponent.getSelectedText();
         lastTextComponent.copy();
      }
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Edit | Paste
   private void menuEditPaste_actionPerformed(ActionEvent e)
   {
      if (lastTextComponent != null)
      {
//         String s = lastTextComponent.getSelectedText();
         lastTextComponent.paste();
      }
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Edit | Select All
   private void menuSelectAll_actionPerformed(ActionEvent e)
   {
      if (lastTextComponent != null)
         lastTextComponent.selectAll();
      else
         Toolkit.getDefaultToolkit().beep();
   }

   // Row | Add
   void tableMenuRowAdd_actionPerformed(ActionEvent e)
   {
      DefaultTableModel model = (DefaultTableModel)table.getModel();
      model.addRow(new Vector(model.getColumnCount()));
      Rectangle r = this.getBounds();
      table.setPreferredScrollableViewportSize(table.getPreferredSize());
      table.revalidate();
      table.doLayout();
      table.repaint();
      this.pack();
      this.setBounds(r.x, r.y, r.width, r.height);
   }

   // Row | Insert
   void tableMenuRowInsert_actionPerformed(ActionEvent e)
   {
      DefaultTableModel model = (DefaultTableModel)table.getModel();
      int curRow = table.getSelectedRow();
      if (curRow < 0) curRow = 0;

      // Java bug. Must stop editting or value in cell will be copied
      if (table.isEditing())
         ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();

      model.insertRow(curRow, new Vector(model.getColumnCount()));
      Rectangle r = this.getBounds();
      table.setPreferredScrollableViewportSize(table.getPreferredSize());
      table.revalidate();
      table.doLayout();
      table.repaint();
      this.pack();
      this.setBounds(r.x, r.y, r.width, r.height);
   }

   // Row | Delete
   void tableMenuRowDelete_actionPerformed(ActionEvent e)
   {
      DefaultTableModel model = (DefaultTableModel)table.getModel();
      if (table.getSelectedRow() >= 0)
      {
         if (table.getEditingRow() == table.getSelectedRow())
            ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();

         model.removeRow(table.getSelectedRow());
         table.revalidate();
         table.repaint();
      }
      else
         JOptionPane.showMessageDialog(app, "Click on a cell in the row you want to delete",
            "Delete Row", JOptionPane.INFORMATION_MESSAGE);

   }

   // Row | Expand All
   void tableMenuRowExpandAll_actionPerformed(ActionEvent e)
   {
      // This causes TextBoxCellEditor to get called on every cell
      for (int r = 0 ; r < table.getRowCount() ; r++)
         for (int c = 0 ; c < table.getColumnCount() ; c++)
            table.editCellAt(r, c);
      Rectangle r = this.getBounds();
      table.setPreferredScrollableViewportSize(table.getPreferredSize());
      table.revalidate();
      table.doLayout();
      table.repaint();
      this.pack();
      this.setBounds(r.x, r.y, r.width, r.height);
   }

   // Column | Add
   void tableMenuColumnAdd_actionPerformed(ActionEvent e)
   {
      // Get new column name
      String newName = JOptionPane.showInputDialog("Enter new column name:");

      // And add it
      if (newName.length() > 0)
      {
         DefaultTableModel model = (DefaultTableModel)table.getModel();
         model.addColumn(newName);

         // Make it the first column
         table.getColumnModel().moveColumn(table.getColumnCount()-1, 0);

         table.revalidate();
         table.repaint();
      }
   }

   // Column | Rename
   void tableMenuColumnRename_actionPerformed(ActionEvent e)
   {
      if (table.getSelectedColumn() >= 0)
      {
         // Get old and new column names
         String oldName = table.getColumnName(table.getSelectedColumn());
         String newName = JOptionPane.showInputDialog("Rename " + oldName + " column to:");

         if (newName.length() > 0)
         {
            Vector colNames = getTableColumnNames(table);
            int idx = colNames.indexOf(oldName);
            colNames.setElementAt(newName, idx);
            DefaultTableModel model = (DefaultTableModel)table.getModel();
            model.setColumnIdentifiers(colNames);

            table.revalidate();
            table.repaint();
         }
      }
      else
         JOptionPane.showMessageDialog(app, "Click on a row under the column you want to rename",
            "Rename Column", JOptionPane.INFORMATION_MESSAGE);
   }

   // Column | Delete
   void tableMenuColumnDelete_actionPerformed(ActionEvent e)
   {
      if (table.getSelectedColumn() >= 0)
      {
         TableColumn col = table.getColumn(table.getColumnName(table.getSelectedColumn()));
         table.removeColumn(col);
         table.revalidate();
         table.repaint();
      }
      else
         JOptionPane.showMessageDialog(app, "Click on a row under the column you want to delete",
            "Delete Column", JOptionPane.INFORMATION_MESSAGE);
   }

   // Help | Object
   void menuHelpObject_actionPerformed(ActionEvent e)
   {
      helpButton_actionPerformed(e);
   }

   // Popup | Insert object name
   void popupInsertName_actionPerformed(ActionEvent e)
   {
   }

   // Popup | Open object
   void popupOpenObject_actionPerformed(ActionEvent e)
   {
      String objectName;
      int cursor;

      if (lastTextComponent == null)
      {
         Toolkit.getDefaultToolkit().beep();
         return;
      }
      cursor = lastTextComponent.getCaretPosition();
      if (cursor < 0)
      {
         Toolkit.getDefaultToolkit().beep();
         return;
      }

      // See if the user highlighted a name
      if (lastTextComponent.getSelectedText() != null)
         objectName = lastTextComponent.getSelectedText();
      else
      {
         // If not, get the name in percents
         objectName = Utils.getObjectName(lastTextComponent.getText(), cursor, '%');
         if (objectName == null)
         {
            // If not, get the name in single quotes
            objectName =  Utils.getObjectName(lastTextComponent.getText(), cursor, '\'');
            if (objectName == null)
               // If not, get the name between spaces
               objectName =  Utils.getObjectName(lastTextComponent.getText(), cursor, ' ');
         }
      }

      // Couldn't find anything to try
      if (objectName == null)
      {
         Toolkit.getDefaultToolkit().beep();
         return;
      }

      try
      {
         // See if it exists
         String objectPath = object.kb.findObject(objectName);

         // If not, ask if the user wants to create it
         if (objectPath == null)
         {
            if (JOptionPane.showConfirmDialog(app, "Object " + objectName + " does not exist. Create it?",
               "Object New", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
                  tree.newObject(objectName);
            return;
         }
         // Otherwise just open it
         else
         {
            tree.editObjectFrame(new KBObject(object.kb, objectPath));
            return;
         }
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // Popup | Make link
   void popupMakeLink_actionPerformed(ActionEvent e)
   {
      if (lastTextComponent == null) return;

      int start = lastTextComponent.getSelectionStart();
      int end = lastTextComponent.getSelectionEnd();
      if (start == end)
      {
         Toolkit.getDefaultToolkit().beep();
         return;
      }
      Document doc = lastTextComponent.getDocument();
      try
      {
         doc.insertString(end, "%", null);
         doc.insertString(start, "%", null);
      }
      catch (BadLocationException ex)
      {
         Toolkit.getDefaultToolkit().beep();
      }
   }

   // Button | Help
   void helpButton_actionPerformed(ActionEvent e)
   {
      try
      {
         String s = "file://" + docsDirectory + "jigs" + System.getProperty("file.separator") +
            object.getKB().getJig() + System.getProperty("file.separator") + object.getHelpURL();
         Utils.displayURL(settings.get("web_browser"), s);
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(app, "Unable to open web browser specified in Preferences to display help:" +
            settings.get("web_browser") + "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
      }
   }

   // Button | Save
   void saveButton_actionPerformed(ActionEvent e)
   {
      saveSlots(editPanel);
   }

   // Button | Save & Close
   void saveCloseButton_actionPerformed(ActionEvent e)
   {
      if (saveSlots(editPanel))
         closeButton_actionPerformed(null);
   }

   // Button Close
   void closeButton_actionPerformed(ActionEvent e)
   {
      // Kludge--Stupid Java tosses the value if the user doesn't select
      // another cell or press <enter>
      if (table != null)
      {
         if (table.isEditing())
            ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
      }

      if (okToClose(JOptionPane.YES_NO_OPTION))
      {
         tree.childFrameClosed(this);
         dispose();
      }
   }

//----------------------------------------------------------------------------//

   // Utilities

   private void setDirty(boolean state)
   {
      dirty = state;
      if (dirty)
         this.setTitle(object.getName() + " *");
      else
         this.setTitle(object.getName());
   }

//----------------------------------------------------------------------------//

   // Public Methods

   public KBObject getKBObject()
   {
      return object;
   }

   public boolean okToClose(int option)
   {
      // Kludge--Stupid Java tosses the value if the user doesn't select
      // another cell or press <enter>
      if (table != null)
      {
         if (table.isEditing())
            ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
      }

      if (!dirty)
      {
         return true;
      }

      // Confirm close w/o saving
      try
      {
         this.setSelected(true);
      }
      catch (Exception ex)
      {
      }
      this.show();

      int value =  JOptionPane.showConfirmDialog(this, res.getString("SaveObjectChanges.Prompt") +
         " " + object.getName(), res.getString("buttonClose.Text"), option) ;
      switch (value)
      {
         case JOptionPane.YES_OPTION:
            if (saveSlots(editPanel))
               return true;
            else
               return false;
         case JOptionPane.NO_OPTION:
            return true;
         case JOptionPane.CANCEL_OPTION:
         default:
            return false;
      }
   }

//----------------------------------------------------------------------------//

   // Overridden Methods

   // This piece of code causes the cursor to not disappear when moving
   // between different opened object windows. It must be overridden for
   // every JInternalFrame that uses text components.
   // This is documented on java.sun.com bug id 4309079

   public void moveToFront()
   {
      Window window = SwingUtilities.getWindowAncestor(this);
      Component focusOwner = (window != null) ? window.getFocusOwner() :
                             null;
      boolean descendant = false;

      if (window != null && focusOwner != null &&
                   SwingUtilities.isDescendingFrom(focusOwner, this)) {
         descendant = true;
         requestFocus();
      }

      super.moveToFront();

      if (descendant) {
         focusOwner.requestFocus();
      }
   }

//----------------------------------------------------------------------------//

   // FocusListener
/*
   public void focusGained(FocusEvent e)
   {
   }

   public void focusLost(FocusEvent e)
   {
      if (table.isEditing())
      {
         int r = table.getEditingRow();
         int c = table.getEditingColumn();
         ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
         table.editCellAt(r, c);
      }
   }
*/
//----------------------------------------------------------------------------//

   // ComponentListener
/*
   public void componentHidden(ComponentEvent e)
   {
   }

   public void componentMoved(ComponentEvent e)
   {
   }

   public void componentResized(ComponentEvent e)
   {
      System.out.println(e.getComponent());
      if (table.isEditing())
         ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
   }

   public void componentShown(ComponentEvent e)
   {
   }
*/
//----------------------------------------------------------------------------//

   // TableColumnModelListener

   public void columnAdded(TableColumnModelEvent e)
   {
   }

   public void columnRemoved(TableColumnModelEvent e)
   {
   }

   public void columnMoved(TableColumnModelEvent e)
   {
   }

   public void columnMarginChanged(ChangeEvent e)
   {
      if (table.isEditing())
      {
         int r = table.getEditingRow();
         int c = table.getEditingColumn();
         ((AbstractCellEditor)table.getCellEditor()).stopCellEditing();
         table.editCellAt(r, c);
      }
   }

   public void columnSelectionChanged(ListSelectionEvent e)
   {
   }

//----------------------------------------------------------------------------//

   // KeyListener

   public void keyPressed(KeyEvent e)
   {
   }

   public void keyReleased(KeyEvent e)
   {
// Another approach--use it with a toolbar
//    helpButton.registerKeyboardAction(toolbarListener,
//                              KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0), WHEN_IN_FOCUSED_WINDOW);

      if (e.getKeyCode() == KeyEvent.VK_F1 || e.getKeyCode() == KeyEvent.VK_HELP)
      {
         helpButton_actionPerformed(null);
      }
   }

   public void keyTyped(KeyEvent e)
   {
   }

//----------------------------------------------------------------------------//

   // DocumentListener

   public void insertUpdate(DocumentEvent e)
   {
      setDirty(true);
//      for (int i = 1 ; i < editPanel.getComponentCount() ; i += 2)
//         if (editPanel.getComponent(i).hasFocus())
//            lastTextComponent = editPanel.getComponent(i);
   }
   public void removeUpdate(DocumentEvent e)
   {
      setDirty(true);
   }
   public void changedUpdate(DocumentEvent e)
   {
      setDirty(true);
   }

//----------------------------------------------------------------------------//

   // TableModelListener

   public void tableChanged(TableModelEvent e)
   {
      if (e.getType() == TableModelEvent.INSERT || e.getType() == TableModelEvent.UPDATE
         || e.getType() == TableModelEvent.DELETE)
      {
      setDirty(true);
      }
   }

//----------------------------------------------------------------------------//

   // ListDataListener

   public void contentsChanged(ListDataEvent e)
   {
      setDirty(true);
   }

   public void intervalAdded(ListDataEvent e)
   {
      setDirty(true);
   }

   public void intervalRemoved(ListDataEvent e)
   {
      setDirty(true);
   }

//----------------------------------------------------------------------------//

   // MouseListener

   public void mouseClicked(MouseEvent e) {
   }
   public void mousePressed(MouseEvent e)
   {
      if (e.isPopupTrigger())
         popup.show(e.getComponent(), e.getX(), e.getY());
   }
   public void mouseReleased(MouseEvent e)
   {
      if (e.isPopupTrigger())
         popup.show(e.getComponent(), e.getX(), e.getY());
   }
   public void mouseEntered(MouseEvent e) {
   }
   public void mouseExited(MouseEvent e) {
   }

//----------------------------------------------------------------------------//

   // MenuItemListener

   class MenuItemListener implements ItemListener
   {
      private JPanel panel;
      private JComboBox menu;
      private String slot;
      private KBObject object;

      public MenuItemListener(JPanel panel, JComboBox menu, KBObject object, String slot)
      {
         this.panel = panel;
         this.menu = menu;
         this.object = object;
         this.slot = slot;
      }

      public void itemStateChanged(ItemEvent e)
      {
         if (e.getStateChange() == ItemEvent.SELECTED)
         {
            setDirty(true);

            try
            {
               String choice = (String)e.getItem();
               Vector v = object.getMenuRelatedSlots(slot, choice);
               for (int i = 0 ; i < v.size() ; i++)
                  setSlotVisible(panel, (String)v.elementAt(i), true);
               v = object.getMenuUnrelatedSlots(slot, choice);
               for (int i = 0 ; i < v.size() ; i++)
                  setSlotVisible(panel, (String)v.elementAt(i), false);
               panel.validate();
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            }
         }
      }
   }

//----------------------------------------------------------------------------//

   // List Box Add, Change, Delete, Up, Down Listeners

   class DelButtonListener implements ActionListener
   {
      private JList list;
      private JButton delButton, upButton, downButton;

      public DelButtonListener(JButton delButton, JButton upButton, JButton downButton, JList list)
      {
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         DefaultListModel listModel = (DefaultListModel)list.getModel();
         ListSelectionModel lsm = list.getSelectionModel();
         int firstSelected = lsm.getMinSelectionIndex();
         int lastSelected = lsm.getMaxSelectionIndex();
         if (firstSelected >= 0 && lastSelected >= 0)
         {
            listModel.removeRange(firstSelected, lastSelected);
            setDirty(true);
         }
         else
         {
            Toolkit.getDefaultToolkit().beep();
            return;
         }

         int size = listModel.size();
         if (size == 0)
         {
            // List is empty: disable delete, up, and down buttons.
            delButton.setEnabled(false);
            upButton.setEnabled(false);
            downButton.setEnabled(false);
         }
         else
         {
            // Adjust the selection.
            if (firstSelected == listModel.getSize())
            {
               // Removed item in last position.
               firstSelected--;
            }
            list.setSelectedIndex(firstSelected);
         }
      }
   }

   // Listener for add button and text field
   class AddButtonListener implements ActionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JList list;
      private JTextField addText;

      public AddButtonListener(JButton addButton, JButton chgButton, JButton delButton,
         JButton upButton, JButton downButton, JTextField addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         DefaultListModel listModel = (DefaultListModel)list.getModel();

         if (addText.getText().equals(""))
         {
            // User didn't type in an item to add
            Toolkit.getDefaultToolkit().beep();
            return;
         }

         int index = list.getSelectedIndex();
         int size = listModel.getSize();

         // If no selection or if item in last position is selected,
         // add the new one to end of list, and select new one.
         if (index == -1 || (index+1 == size) || size == 0)
         {
             listModel.addElement(addText.getText());
             list.setSelectedIndex(size);
             setDirty(true);
         }
         // Otherwise insert the new one after the current selection,
         // and select new one.
         else
         {
             listModel.insertElementAt(addText.getText(), index+1);
             list.setSelectedIndex(index+1);
             setDirty(true);
         }
         // Clear the add text so you can enter items rapidly
         addText.setText(null);
         delButton.setEnabled(true);
         upButton.setEnabled(true);
         downButton.setEnabled(true);
      }
   }

   // Listener for add button and text field
   class ChangeButtonListener implements ActionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JList list;
      private JTextField addText;

      public ChangeButtonListener(JButton addButton, JButton chgButton, JButton delButton,
         JButton upButton, JButton downButton, JTextField addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         DefaultListModel listModel = (DefaultListModel)list.getModel();

         int index = list.getSelectedIndex();
         int size = listModel.getSize();

         // User didn't select an item to replace and type in the text
         if (addText.getText().equals("") || index == -1 || size == 0)
         {
            Toolkit.getDefaultToolkit().beep();
            return;
         }

         // Replace the selected element with the new text
         listModel.setElementAt(addText.getText(), index);
         list.setSelectedIndex(index);
         setDirty(true);

         // Clear the add text so you can enter items rapidly
         addText.setText(null);
         chgButton.setEnabled(true);
         delButton.setEnabled(true);
         upButton.setEnabled(true);
         downButton.setEnabled(true);
      }
   }

   // Listen for clicks on the up and down arrow buttons.
   class UpDownListener implements ActionListener
   {
      private JButton upButton, downButton;
      private JList list;

      public UpDownListener(JButton upButton, JButton downButton, JList list)
      {
         this.upButton = upButton;
         this.downButton = downButton;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         DefaultListModel listModel = (DefaultListModel)list.getModel();

         int moveMe = list.getSelectedIndex();
         if (moveMe == -1 || listModel.size() <= 1)
            Toolkit.getDefaultToolkit().beep();

         // up arrow button
         if (e.getActionCommand().equals(upString))
         {
            if (moveMe != 0)
            {
               // not already at top
               swap(listModel, moveMe, moveMe-1);
               list.setSelectedIndex(moveMe-1);
               list.ensureIndexIsVisible(moveMe-1);
               setDirty(true);
            }
         }
         // down arrow button
         else
         {
            if (moveMe != listModel.getSize()-1)
            {
               // not already at bottom
               swap(listModel, moveMe, moveMe+1);
               list.setSelectedIndex(moveMe+1);
               list.ensureIndexIsVisible(moveMe+1);
               setDirty(true);
            }
         }
      }

      // Swap two elements in the list.
      private void swap(DefaultListModel listModel, int a, int b)
      {
         Object aObject = listModel.getElementAt(a);
         Object bObject = listModel.getElementAt(b);
         listModel.set(a, bObject);
         listModel.set(b, aObject);
         setDirty(true);
      }
   }

//----------------------------------------------------------------------------//

   // ListSelectionListener (for ListBoxes)

   class ListListener implements ListSelectionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JTextField addText;
      private JList list;

      public ListListener(JButton addButton, JButton chgButton, JButton delButton, JButton upButton,
         JButton downButton, JTextField addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      //Listener method for list selection changes.
      public void valueChanged(ListSelectionEvent e)
      {
        if (e.getValueIsAdjusting() == false)
        {
            if (list.getSelectedIndex() == -1 || list.getModel().getSize() == 0)
            {
               //No selection: disable delete, up, and down buttons.
               chgButton.setEnabled(false);
               delButton.setEnabled(false);
               upButton.setEnabled(false);
               downButton.setEnabled(false);
               addText.setText("");
            }
            else
               if (list.getSelectedIndices().length > 1)
               {
                  //Multiple selection: disable up and down buttons.
                  chgButton.setEnabled(false);
                  delButton.setEnabled(true);
                  upButton.setEnabled(false);
                  downButton.setEnabled(false);
               }
               else
               {
                  //Single selection: permit all operations.
                  chgButton.setEnabled(true);
                  delButton.setEnabled(true);
                  upButton.setEnabled(true);
                  downButton.setEnabled(true);
                  addText.setText(list.getSelectedValue().toString());
               }
         }
      }
   }

//----------------------------------------------------------------------------//

   // Combo List Box Add, Change Listeners

   // Listener for add button and text field
   class AddButtonComboListener implements ActionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JList list;
      private JComboBox addText;

      public AddButtonComboListener(JButton addButton, JButton chgButton, JButton delButton,
         JButton upButton, JButton downButton, JComboBox addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         if (e.getActionCommand().equals("comboBoxChanged") && addText.getSelectedIndex() < 0) return;

         DefaultListModel listModel = (DefaultListModel)list.getModel();

         if (((String)addText.getSelectedItem()).equals(""))
         {
            // User didn't type in an item to add
            Toolkit.getDefaultToolkit().beep();
            return;
         }

         int index = list.getSelectedIndex();
         int size = listModel.getSize();

         // If no selection or if item in last position is selected,
         // add the new one to end of list, and select new one.
         if (index == -1 || (index+1 == size) || size == 0)
         {
            listModel.addElement((String)addText.getSelectedItem());
            list.setSelectedIndex(size);
            setDirty(true);
         }
         // Otherwise insert the new one after the current selection,
         // and select new one.
         else
         {
            listModel.insertElementAt((String)addText.getSelectedItem(), index+1);
            list.setSelectedIndex(index+1);
            setDirty(true);
         }
         // Clear the add text so you can enter items rapidly
         addText.getEditor().setItem("");
         delButton.setEnabled(true);
         upButton.setEnabled(true);
         downButton.setEnabled(true);
      }
   }

   // Listener for add button and text field
   class ChangeButtonComboListener implements ActionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JList list;
      private JComboBox addText;

      public ChangeButtonComboListener(JButton addButton, JButton chgButton, JButton delButton,
         JButton upButton, JButton downButton, JComboBox addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      public void actionPerformed(ActionEvent e)
      {
         DefaultListModel listModel = (DefaultListModel)list.getModel();

         int index = list.getSelectedIndex();
         int size = listModel.getSize();

         // User didn't select an item to replace and type in the text
         if (((String)addText.getSelectedItem()).equals("") || index == -1 || size == 0)
         {
            Toolkit.getDefaultToolkit().beep();
            return;
         }

         // Replace the selected element with the new text
         listModel.setElementAt((String)addText.getSelectedItem(), index);
         list.setSelectedIndex(index);
         setDirty(true);

         // Clear the add text so you can enter items rapidly
         addText.getEditor().setItem("");
         chgButton.setEnabled(true);
         delButton.setEnabled(true);
         upButton.setEnabled(true);
         downButton.setEnabled(true);
      }
   }


//----------------------------------------------------------------------------//

   // ListSelectionListener (for ListComboBoxes)

   class ListComboListener implements ListSelectionListener
   {
      private JButton addButton, chgButton, delButton, upButton, downButton;
      private JComboBox addText;
      private JList list;

      public ListComboListener(JButton addButton, JButton chgButton, JButton delButton, JButton upButton,
         JButton downButton, JComboBox addText, JList list)
      {
         this.addButton = addButton;
         this.chgButton = chgButton;
         this.delButton = delButton;
         this.upButton = upButton;
         this.downButton = downButton;
         this.addText = addText;
         this.list = list;
      }

      //Listener method for list selection changes.
      public void valueChanged(ListSelectionEvent e)
      {
        if (e.getValueIsAdjusting() == false)
        {
            if (list.getSelectedIndex() == -1 || list.getModel().getSize() == 0)
            {
               //No selection: disable delete, up, and down buttons.
               chgButton.setEnabled(false);
               delButton.setEnabled(false);
               upButton.setEnabled(false);
               downButton.setEnabled(false);
               addText.getEditor().setItem("");
            }
            else
               if (list.getSelectedIndices().length > 1)
               {
                  //Multiple selection: disable up and down buttons.
                  chgButton.setEnabled(false);
                  delButton.setEnabled(true);
                  upButton.setEnabled(false);
                  downButton.setEnabled(false);
               }
               else
               {
                  //Single selection: permit all operations.
                  chgButton.setEnabled(true);
                  delButton.setEnabled(true);
                  upButton.setEnabled(true);
                  downButton.setEnabled(true);
                  addText.getEditor().setItem(list.getSelectedValue().toString());
               }
         }
      }
   }

//----------------------------------------------------------------------------//

   // ListSelectionListener (for Tables)

   class TableListener implements ListSelectionListener
   {
      private JTable table;
      private boolean undoAdded = false;

      public TableListener(JTable table)
      {
         this.table = table;
      }

      // Listener method for list selection changes.
      public void valueChanged(ListSelectionEvent e)
      {
         if (e.getValueIsAdjusting() == false)
         {
            ListSelectionModel lsm = (ListSelectionModel)e.getSource();
            if (lsm.isSelectionEmpty()) return;

            // Save this component for cut, copy and paste
            // If it's a TEXT_TABLE then we need to get the JTextArea inside the scroll pane
            if (table.getEditorComponent() != null && table.getEditorComponent().getClass() == JScrollPane.class)
               lastTextComponent = (JTextComponent)((JScrollPane)table.getEditorComponent()).getViewport().getComponent(0);
            // Otherwise if it's a TABLE then just get the JTextField
            else
               lastTextComponent = (JTextComponent)table.getEditorComponent();
         }
      }
   }

//----------------------------------------------------------------------------//

   // UndoableEditListener

   class UndoHandler implements UndoableEditListener
   {
   /**
	 * Messaged when the Document has created an edit, the edit is
	 * added to <code>undo</code>, an instance of UndoManager.
	 */
      public void undoableEditHappened(UndoableEditEvent e)
      {
         undo.addEdit(e.getEdit());
//       undoAction.update();
//       redoAction.update();
      }
   }

//----------------------------------------------------------------------------//

   // FocusListener

   class TextboxFocusListener implements FocusListener
   {
      private KBObjectFrame frame;
      private JTextArea textbox;

      public TextboxFocusListener(KBObjectFrame frame, JTextArea textbox)
      {
         this.frame = frame;
         this.textbox = textbox;
      }

      public void focusGained(FocusEvent e)
      {
//         System.out.println("gained");
         lastTextComponent = textbox;
//         int cnt = textbox.getLineCount();
//         textbox.setRows(textbox.getLineCount()+1);
//         frame.validate();

//         scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
//         scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      }

      public void focusLost(FocusEvent e)
      {
//         System.out.println("lost");
//         scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
//         scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
         lastTextComponent = null;
      }
   }

//----------------------------------------------------------------------------//

   // InternalFrameListener

   public void internalFrameOpened(InternalFrameEvent e)
   {
   }

   public void internalFrameClosing(InternalFrameEvent e)
   {
      closeButton_actionPerformed(null);
   }

   public void internalFrameClosed(InternalFrameEvent e)
   {
   }

   public void internalFrameIconified(InternalFrameEvent e)
   {
   }

   public void internalFrameDeiconified(InternalFrameEvent e)
   {
   }

   public void internalFrameActivated(InternalFrameEvent e)
   {
   }

   public void internalFrameDeactivated(InternalFrameEvent e)
   {
   }

//----------------------------------------------------------------------------//

   // ComponentListener

   public void componentResized(ComponentEvent e)
   {
      pack();
   }

   public void componentMoved(ComponentEvent e)
   {
   }

   public void componentShown(ComponentEvent e)
   {
   }

   public void componentHidden(ComponentEvent e)
   {
   }

}
