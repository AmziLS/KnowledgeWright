
//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package  amzi.ide;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

public class KBTreeFrame extends JInternalFrame
   implements /*TreeSelectionListener,*/ ChangeListener, KeyListener, MouseListener
{
   private ResourceBundle res = ResourceBundle.getBundle("amzi.ide.resources.IDE");
   private JMenuBar treeMenuBar = new JMenuBar();
   private JMenu menuObject = new JMenu();
   private JMenuItem menuObjectEdit = new JMenuItem();
   private JMenuItem menuObjectNew = new JMenuItem();
   private JMenuItem menuObjectRename = new JMenuItem();
   private JMenuItem menuObjectDelete = new JMenuItem();
   private JMenuItem menuObjectMove = new JMenuItem();
   private JMenuItem menuObjectCopy = new JMenuItem();
   private JMenuItem menuObjectPrintTree = new JMenuItem();
   private JMenu menuFolder = new JMenu();
   private JMenuItem menuFolderNew = new JMenuItem();
   private JMenuItem menuFolderRename = new JMenuItem();
   private JMenuItem menuFolderDelete = new JMenuItem();
   private JMenuItem menuFolderMove = new JMenuItem();
   private JMenuItem menuFolderPrintTree = new JMenuItem();
   private BorderLayout borderLayout1 = new BorderLayout();
   private JTabbedPane kbTabbedPane = new JTabbedPane();
   private JScrollPane kbObjectScrollPane = new JScrollPane();
   private JTree kbObjectTree;
   private JScrollPane kbFolderScrollPane = new JScrollPane();
   private JTree kbFolderTree;
   private JPanel buttonPanel = new JPanel();
   private JButton newButton = new JButton();
   private JButton editButton = new JButton();
   private JButton renameButton = new JButton();
   private JButton deleteButton = new JButton();
   private JPopupMenu popup = new JPopupMenu();
   private JMenuItem popupEdit = new JMenuItem();
   private JMenuItem popupRename = new JMenuItem();
   private JMenuItem popupDelete = new JMenuItem();
   private JMenuItem popupCopy = new JMenuItem();
   private JMenuItem popupMove = new JMenuItem();

   private KB kb;
   private App app;
   private StatusTabFrame statusFrame;
   private String docsDirectory;
   private DefaultMutableTreeNode objectRoot, folderRoot;
   private Vector objectList = new Vector();
   private int lastXloc = 0, lastYloc = 0;
   private int frameBottom;
   private SavedSettings settings;

   public KBTreeFrame(App app, StatusTabFrame statusFrame, KB kb, SavedSettings settings, String docsDirectory)
   {
      super(kb.getName(), true, true, true, true);
      this.app = app;
      this.statusFrame = statusFrame;
      this.kb = kb;
      this.settings = settings;
      this.docsDirectory = docsDirectory;
      try
      {
         jbInit();
         frameBottom = statusFrame.getLocation().y;
         app.addOpenFile(new OpenFile(kb.getPath(), this, kb));
     }
      catch (Exception ex)
      {
         ex.printStackTrace();
      }
   }

//----------------------------------------------------------------------------//
//                               Initializers                                 //

   private void jbInit() throws Exception
   {
      this.setVisible(true);
      this.setClosable(false);
      this.setIconifiable(true);
      this.setMaximizable(true);
      this.setResizable(true);
      this.getContentPane().setLayout(borderLayout1);
      ImageIcon icon = new ImageIcon(getClass().getResource("resources/ide_icon.gif"));
      this.setFrameIcon(icon);

      // Object Menu
      menuObject.setText("Object");
      menuObject.setMnemonic(KeyEvent.VK_O);

      menuObjectEdit.setText("Edit");
      menuObjectEdit.setMnemonic(KeyEvent.VK_E);
      menuObjectEdit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK));
      menuObjectEdit.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectEdit_actionPerformed(e);
         }
      });

      menuObjectNew.setText("New");
      menuObjectNew.setMnemonic(KeyEvent.VK_N);
      menuObjectNew.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
      menuObjectNew.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectNew_actionPerformed(e);
         }
      });

      menuObjectRename.setText("Rename");
      menuObjectRename.setMnemonic(KeyEvent.VK_R);
      menuObjectRename.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectRename_actionPerformed(e);
         }
      });

      menuObjectDelete.setText("Delete");
      menuObjectDelete.setMnemonic(KeyEvent.VK_D);
      menuObjectDelete.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectDelete_actionPerformed(e);
         }
      });

      menuObjectCopy.setText("Copy");
      menuObjectCopy.setMnemonic(KeyEvent.VK_C);
      menuObjectCopy.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectCopy_actionPerformed(e);
         }
      });

      menuObjectMove.setText("Move");
      menuObjectMove.setMnemonic(KeyEvent.VK_M);
      menuObjectMove.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectMove_actionPerformed(e);
         }
      });

      menuObjectPrintTree.setText("Print Tree");
      menuObjectPrintTree.setMnemonic(KeyEvent.VK_P);
      menuObjectPrintTree.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectPrintTree_actionPerformed(e);
         }
      });

      menuObject.add(menuObjectEdit);
      menuObject.addSeparator();
      menuObject.add(menuObjectNew);
      menuObject.add(menuObjectRename);
      menuObject.add(menuObjectDelete);
      menuObject.addSeparator();
      menuObject.add(menuObjectCopy);
      menuObject.add(menuObjectMove);
      menuObject.addSeparator();
      menuObject.add(menuObjectPrintTree);

      // Folder Menu
      menuFolder.setText("Folder");
      menuFolder.setMnemonic(KeyEvent.VK_F);

      menuFolderNew.setText("New");
      menuFolderNew.setMnemonic(KeyEvent.VK_N);
      menuFolderNew.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuFolderNew_actionPerformed(e);
         }
      });

      menuFolderRename.setText("Rename");
      menuFolderRename.setMnemonic(KeyEvent.VK_R);
      menuFolderRename.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuFolderRename_actionPerformed(e);
         }
      });

      menuFolderDelete.setText("Delete");
      menuFolderDelete.setMnemonic(KeyEvent.VK_D);
      menuFolderDelete.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuFolderDelete_actionPerformed(e);
         }
      });

      menuFolderMove.setText("Move");
      menuFolderMove.setMnemonic(KeyEvent.VK_M);
      menuFolderMove.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuFolderMove_actionPerformed(e);
         }
      });

      menuFolderPrintTree.setText("Print Tree");
      menuFolderPrintTree.setMnemonic(KeyEvent.VK_P);
      menuFolderPrintTree.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuFolderPrintTree_actionPerformed(e);
         }
      });

      menuFolder.add(menuFolderNew);
      menuFolder.add(menuFolderRename);
      menuFolder.add(menuFolderDelete);
      menuFolder.addSeparator();
      menuFolder.add(menuFolderMove);
      menuFolder.addSeparator();
      menuFolder.add(menuFolderPrintTree);

      // Menubar
      treeMenuBar.add(menuObject);
      treeMenuBar.add(menuFolder);
      this.setJMenuBar(treeMenuBar);

      // Popup Menu
      popupEdit.setText(res.getString("menuEdit.Text"));
      popupEdit.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectEdit_actionPerformed(e);
         }
      });
      popupRename.setText(res.getString("menuRename.Text"));
      popupRename.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            DefaultMutableTreeNode node = getFolderSelection();
            if (node != null)
               menuFolderRename_actionPerformed(e);
            else
               menuObjectRename_actionPerformed(e);
         }
      });
      popupDelete.setText(res.getString("menuDelete.Text"));
      popupDelete.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectDelete_actionPerformed(e);
         }
      });
      popupCopy.setText(res.getString("menuCopy.Text"));
      popupCopy.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectCopy_actionPerformed(e);
         }
      });
      popupMove.setText(res.getString("menuMove.Text"));
      popupMove.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuObjectMove_actionPerformed(e);
         }
      });
      popup.add(popupEdit);
      popup.add(popupRename);
      popup.add(popupDelete);
      popup.add(popupCopy);
      popup.add(popupMove);

      // Fill in the Object Tree
      objectRoot = new DefaultMutableTreeNode("/", true);
      Vector types = kb.getObjectTypes();
      for (int i = 0 ; i < types.size() ; i++)
      {
         DefaultMutableTreeNode typeFolder = new DefaultMutableTreeNode(types.elementAt(i));
         Vector objects = kb.getObjectPaths(types.elementAt(i).toString());
         if (objects != null)
            for (int j = 0 ; j < objects.size() ; j++)
            {
               KBObject kbObject = new KBObject(kb, (String)objects.elementAt(j));
               int index = getObjectIndex(kbObject.getName(), typeFolder);
               typeFolder.insert(new DefaultMutableTreeNode(kbObject, false), index);
            }
         else
            typeFolder.setAllowsChildren(true);
         objectRoot.add(typeFolder);
      }
      kbObjectTree = new JTree(objectRoot, true);
      kbObjectTree.setFont(app.getUserFont());
      kbObjectTree.setRootVisible(false);
      kbObjectTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
      kbObjectTree.setShowsRootHandles(true);
      kbObjectTree.putClientProperty("JTree.lineStyle", "Angled");
//      kbObjectTree.addTreeSelectionListener(this);
      ToolTipManager.sharedInstance().registerComponent(kbObjectTree);
      kbObjectTree.setCellRenderer(new kbTreeRenderer());

      // Fill in the Folder Tree
      folderRoot = new DefaultMutableTreeNode("/", true);

      // First the folderless objects
      Vector nofolders = kb.getRootObjects();
      for (int i = 0 ; i < nofolders.size() ; i++)
      {
         DefaultMutableTreeNode n = new DefaultMutableTreeNode(new KBObject(kb, " / " + nofolders.elementAt(i)));
         n.setAllowsChildren(false);
         folderRoot.add(n);
      }

      // Then the folders
      Vector folders = kb.getRootFolders();
      for (int i = 0 ; i < folders.size() ; i++)
      {
         DefaultMutableTreeNode n = new DefaultMutableTreeNode(folders.elementAt(i));
         n.setAllowsChildren(true);
         buildFolderSubtree(n);
         folderRoot.add(n);
      }

      kbFolderTree = new JTree(folderRoot, true);
      kbFolderTree.setFont(app.getUserFont());
      kbFolderTree.setRootVisible(false);
      kbFolderTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
      kbFolderTree.setShowsRootHandles(true);
      kbFolderTree.putClientProperty("JTree.lineStyle", "Angled");
//      kbFolderTree.addTreeSelectionListener(this);
      ToolTipManager.sharedInstance().registerComponent(kbFolderTree);
      kbFolderTree.setCellRenderer(new kbTreeRenderer());

      // Set up the scrolling tab panes
      // Note, the TreeSelectionListener assumes the Object Tree is first
      this.getContentPane().add(kbTabbedPane, BorderLayout.CENTER);
      kbTabbedPane.addTab("Object View", null, kbObjectScrollPane, "View KB by object types");
      kbTabbedPane.addTab("Folder View", null, kbFolderScrollPane, "View KB by user-defined folders");
      kbTabbedPane.addChangeListener(this);
      kbObjectScrollPane.getViewport().add(kbObjectTree, null);
      kbObjectScrollPane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      kbFolderScrollPane.getViewport().add(kbFolderTree, null);
      kbObjectScrollPane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      stateChanged(null);

      // Add a keyboard listener
      kbObjectTree.addKeyListener(this);
      kbFolderTree.addKeyListener(this);
      kbObjectTree.addMouseListener(this);
      kbFolderTree.addMouseListener(this);
   }

   private String getFolderPath(DefaultMutableTreeNode folder)
   {
      String path = "/" + folder.toString();
      TreeNode n = (TreeNode)folder;

      while (n.getParent() != null)
      {
         path = "/" + n.getParent().toString() + path;
         n = n.getParent();
      }

      return path;
   }

   private void buildFolderSubtree(DefaultMutableTreeNode folder)
   {
      Vector v;

      try
      {
         // Get objects
         v = kb.getFolderObjects(getFolderPath(folder));
         if (v != null)
            for (int j = 0 ; j < v.size() ; j++)
            {
               // Add it
               DefaultMutableTreeNode n = new DefaultMutableTreeNode(new KBObject(kb, getFolderPath(folder) + " / " + v.elementAt(j)));
               folder.add(n);

               // No children
               n.setAllowsChildren(false);
            }

         // Get subfolders
         v = kb.getFolderFolders(getFolderPath(folder));
         if (v != null)
            for (int j = 0 ; j < v.size() ; j++)
            {
               // Add it
               DefaultMutableTreeNode n = new DefaultMutableTreeNode(v.elementAt(j));
               folder.add(n);

               // Add it's children
               n.setAllowsChildren(true);
               buildFolderSubtree(n);
            }
      }
      catch (KBUserException e)
      {
         JOptionPane.showMessageDialog(app, e.getMessage(),
            e.getErrorType(), JOptionPane.ERROR_MESSAGE);
         return;
      }
   }

//----------------------------------------------------------------------------//
//                                 Commands                                   //

   // Object | Edit
   private void menuObjectEdit_actionPerformed(ActionEvent e)
   {
      editObject();
   }

   // Object | New
   private void menuObjectNew_actionPerformed(ActionEvent e)
   {
      newObject(null);
   }

   // Object | Rename
   private void menuObjectRename_actionPerformed(ActionEvent e)
   {
      renameObject();
   }

   // Object | Delete
   private void menuObjectDelete_actionPerformed(ActionEvent e)
   {
      deleteObjectsAndFolders();
   }

   // Object | Copy
   private void menuObjectCopy_actionPerformed(ActionEvent e)
   {
      copyObject();
   }

   // Object | Move
   private void menuObjectMove_actionPerformed(ActionEvent e)
   {
      moveObjectsAndFolders();
   }

   // Object | Print Tree
   private void menuObjectPrintTree_actionPerformed(ActionEvent e)
   {
      Properties prnProp = new Properties();
      PrintJob prnJob = Toolkit.getDefaultToolkit().getPrintJob(app, "Print Knowledgebase", prnProp);
      if (prnJob != null)
      {
         Graphics prnGr = prnJob.getGraphics();

         //Print in the center of the page
         Dimension od = kbObjectTree.getSize(); //Object size
         Dimension pd = prnJob.getPageDimension(); //Page size

         prnGr.translate((pd.width-od.width)/2, (pd.height-od.height)/2);
         kbObjectTree.printAll(prnGr);

         prnGr.dispose(); //Prints here
         prnJob.end(); //Release printer
      }
   }

   // Folder | New
   private void menuFolderNew_actionPerformed(ActionEvent e)
   {
      DefaultTreeModel treeModel;
      DefaultMutableTreeNode typeFolder, node, parent;
      String folderName, folderLocation;

      try
      {
         // Put up a dialog to get all the info
         Vector folders = kb.getAllFolderPaths();
         FieldListDialog fld = new FieldListDialog(app, "New Folder", "Name:", "Parent Folder:", folders, 0);
         Dimension dlgSize = fld.getPreferredSize();
         Dimension frmSize = app.getSize();
         Point loc = getLocation();
         fld.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
         do
         {
            fld.show();
            folderName = fld.getFieldText();
            folderLocation = fld.getListItem();
         }
         while (folderName.length() > 0 && !kb.isValidFolderName(folderName));

         if (folderName.length() > 0)
         {
            // Create the folder
            kb.createFolder(folderLocation, folderName);
            node = new DefaultMutableTreeNode(folderName, true /*allows children*/);

            treeModel = (DefaultTreeModel)kbFolderTree.getModel();
            if (folderLocation.equals("/"))
               parent = folderRoot;
            else
               parent = findNode(treeModel, folderRoot, folderLocation);

            // Add it to the folder tree
            int index = getFolderIndex(folderName, parent);
            treeModel.insertNodeInto(node, parent, index);
         }
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   private void menuFolderRename_actionPerformed(ActionEvent e)
   {
      DefaultTreeModel treeModel;
      DefaultMutableTreeNode treeNode, parent;
      int index;
      String oldPathname, newName;

      DefaultMutableTreeNode node = getFolderSelection();
      if (node != null)
      {
         // Get new name
         do
         {
            newName = JOptionPane.showInputDialog("Enter new folder name:");
         }
         while (newName.length() > 0 && !kb.isValidFolderName(newName));

         // Rename it
         if (newName.length() > 0)
         {
            try
            {
               oldPathname = objectPathToString(node);
               if (kb.renameFolder(oldPathname, newName))
               {
                  // Set the new name in the folder tree
                  treeModel = (DefaultTreeModel)kbFolderTree.getModel();
                  treeNode = findNode(treeModel, folderRoot, oldPathname);
                  parent = (DefaultMutableTreeNode)treeNode.getParent();
                  parent.remove(treeNode);
                  index = getFolderIndex(newName, parent);
                  treeNode.setUserObject((Object)newName);
                  parent.insert(treeNode, index);
                  treeModel.reload(parent);
               }
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               return;
            }
         }
      }
   }

   private void menuFolderDelete_actionPerformed(ActionEvent e)
   {
      deleteObjectsAndFolders();
   }

   private void menuFolderMove_actionPerformed(ActionEvent e)
   {
      moveObjectsAndFolders();
   }

   // Folder | Print Tree
   private void menuFolderPrintTree_actionPerformed(ActionEvent e)
   {
      Properties prnProp = new Properties();
      PrintJob prnJob = Toolkit.getDefaultToolkit().getPrintJob(app, "Print Knowledgebase", prnProp);
      if (prnJob != null)
      {
         Graphics prnGr = prnJob.getGraphics();

         //Print in the center of the page
         Dimension od = kbFolderTree.getSize(); //Object size
         Dimension pd = prnJob.getPageDimension(); //Page size

         prnGr.translate((pd.width-od.width)/2, (pd.height-od.height)/2);
         kbFolderTree.printAll(prnGr);

         prnGr.dispose(); //Prints here
         prnJob.end(); //Release printer
      }
   }
//----------------------------------------------------------------------------//
//                               Command Methods                              //

   public void newObject(String defaultName)
   {
      KBNewObjectDialog obj;
      boolean validName;

      // Put up a dialog to get all the info
      try
      {
         obj = new KBNewObjectDialog(app, kb.getObjectTypes());
         Dimension dlgSize = obj.getPreferredSize();
         Dimension frmSize = app.getSize();
         Point loc = getLocation();
         obj.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
         if (defaultName != null)
            obj.setName(defaultName);
         String type = getSelectionObjectType();
         if (type != null) obj.setType(type);
         do
         {
            obj.show();
            validName = KBObject.isValidObjectName(obj.getName());
            if (!validName)
               JOptionPane.showMessageDialog(app, "Object names must be in single quotes if they include spaces " +
                  "and may not contain special characters", "Error", JOptionPane.ERROR_MESSAGE);
         }
         while (obj.getName().length() > 0 && !validName);

         if (obj.getName().length() > 0 && obj.getType().length() > 0)
         {
            if (obj.getName().charAt(0) == '\'')
               obj.setName(obj.getName().substring(1, obj.getName().length()-1));

            KBObject kbObject = new KBObject(kb, obj.getName(), obj.getType());

            // Add it to the trees
            addObject(kbObject);

            // And edit it
            editObjectFrame(kbObject);
         }
      }
      catch (KBUserException e)
      {
         JOptionPane.showMessageDialog(app, e.getMessage(),
            e.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   private void addObject(KBObject kbObject) throws KBUserException
   {
      DefaultTreeModel treeModel;
      DefaultMutableTreeNode typeFolder, node;

      // Add it to the object tree
      treeModel = (DefaultTreeModel)kbObjectTree.getModel();
      for (int i = 0 ; i < treeModel.getChildCount(objectRoot) ; i ++)
      {
         typeFolder = (DefaultMutableTreeNode)treeModel.getChild(objectRoot, i);
         if (typeFolder.getUserObject().toString().equals(kbObject.getType()))
         {
            node = new DefaultMutableTreeNode(kbObject, false);
            int index = getObjectIndex(kbObject.getName(), typeFolder);
            treeModel.insertNodeInto(node, typeFolder, index);
         }
      }

      // Add it to the folder tree (make a new KBObject)
      treeModel = (DefaultTreeModel)kbFolderTree.getModel();
      node = new DefaultMutableTreeNode(new KBObject(kb, kbObject.getPathname()), false);
      int index = getObjectIndex(kbObject.getName(), folderRoot);
      treeModel.insertNodeInto(node, folderRoot, index);
   }

   private void editObject()
   {
      DefaultMutableTreeNode node = getObjectSelection();

      // See if they selected an object, not a folder
      if (node != null && !node.getAllowsChildren())
      {
         // Clear tree selections (fixes a bug in Java)
//         kbFolderTree.clearSelection();
//         kbObjectTree.clearSelection();

         // Get the kb object
         KBObject kbObject = (KBObject)node.getUserObject();
         editObjectFrame(kbObject);
      }
   }

   // Also called from KBObjectFrame and StatusTabFrame
   public void editObjectFrame(KBObject kbObject)
   {
      // Refresh the status frame
      if (app.getConfigEntry("auto_update_status").equals("true"))
         statusFrame.updateDisplay(kbObject.getType(), kbObject.getPathname());

      // Don't allow the same object to be opened twice
      for (int i = 0 ; i < objectList.size() ; i++)
      {
         KBObjectFrame kbFrame = (KBObjectFrame)objectList.elementAt(i);

         // Check name and type
         if (kbFrame.getKBObject().getPathname().equals(kbObject.getPathname()) &&
            kbFrame.getKBObject().getType().equals(kbObject.getType()) )
         {
            // If it's there bring it to the front
            kbFrame.toFront();
            return;
         }
      }

      if (lastXloc == 0 && lastYloc == 0)
      {
         Rectangle r = this.getBounds();
         lastXloc = r.width;
         lastYloc = 0;
      }
      else if (lastYloc >= 100 || objectList.size() == 0)
         lastYloc = 0;
      else
         lastYloc = lastYloc + 20;

      KBObjectFrame kbFrame = new KBObjectFrame(app, this, kbObject, lastXloc, lastYloc,
         frameBottom, settings, docsDirectory);
      objectList.addElement(kbFrame);
      JDesktopPane desktop = app.getDesktopPane();
//      Dimension deskSize = desktop.getSize();
//      kbFrame.setBounds(200, 0, deskSize.width*1/3, deskSize.height*2/3);
      desktop.add(kbFrame);
      desktop.getDesktopManager().activateFrame(kbFrame);
   }

   private void copyObject()
   {
      DefaultMutableTreeNode node = getObjectSelection();
      if (node != null)
      {
         KBObject oldObject = (KBObject)node.getUserObject();

         // Get new name
         String newName = JOptionPane.showInputDialog(app, "Enter new object name:", "Copy", JOptionPane.QUESTION_MESSAGE);

         try
         {
            // Copy it
            if (newName != null && newName.length() > 0)
            {
               KBObject newObject = new KBObject(kb, oldObject.getPath(), oldObject.getName(),
                  oldObject.getType(), newName);

               // Add it to the trees
               addObject(newObject);

               // And edit it
               editObjectFrame(newObject);
            }
         }
         catch (KBUserException e)
         {
            JOptionPane.showMessageDialog(app, e.getMessage(),
               e.getErrorType(), JOptionPane.ERROR_MESSAGE);
         }
      }
   }

   private void renameObject()
   {
      DefaultTreeModel treeModel;
      DefaultMutableTreeNode treeNode, parent;
      KBObject kbObject;
      int index;
      String oldPathname, oldName;
      boolean isFolder;

      DefaultMutableTreeNode node = getObjectSelection();
      if (node != null)
      {
         kbObject = (KBObject)node.getUserObject();
         oldPathname = kbObject.getPathname();

         // Close it if it's open
         for (int i = 0 ; i < objectList.size() ; i++)
         {
            KBObjectFrame objFrame = ((KBObjectFrame)objectList.elementAt(i));
            if (objFrame.getKBObject().getPathname().equals(oldPathname))
            {
               objFrame.okToClose(JOptionPane.YES_NO_OPTION);
               objFrame.dispose();
            }
         }

         // Get new name
         String newName = JOptionPane.showInputDialog(app, "Enter new object name:", "Rename", JOptionPane.QUESTION_MESSAGE);

         // Rename it
         if (newName != null && newName.length() > 0)
         {
            try
            {
               oldName = kbObject.getName();
               isFolder = node.getAllowsChildren();

               if (kbObject.rename(newName))
               {
                  // And set the new name in both trees -- first the object tree
                  treeModel = (DefaultTreeModel)kbObjectTree.getModel();

                  // Since we just changed the kbObject, search for the old name or new name depending upon
                  // which tree was currently selected
                  if (kbTabbedPane.getSelectedIndex() == 0)    // Object tree selected
                     treeNode = findNode(treeModel, objectRoot, "/ " + kbObject.getType() + " / " + newName);
                  else                                         // Folder tree selected
                     treeNode = findNode(treeModel, objectRoot, "/ " + kbObject.getType() + " / " + oldName);
                  parent = (DefaultMutableTreeNode)treeNode.getParent();
                  parent.remove(treeNode);
                  index = getObjectIndex(newName, parent);

                  // Always create fresh new objects to avoid bugs
                  parent.insert(new DefaultMutableTreeNode(new KBObject(kb, kbObject.getPathname()), isFolder), index);
                  treeModel.reload(parent);

                  // Then the folder tree
                  treeModel = (DefaultTreeModel)kbFolderTree.getModel();

                  // Since we just changed the kbObject, search for the old name or new name depending upon
                  // which tree was currently selected
                  if (kbTabbedPane.getSelectedIndex() == 0)    // Object tree selected
                     treeNode = findNode(treeModel, folderRoot, oldPathname);
                  else                                         // Folder tree selected
                     treeNode = findNode(treeModel, folderRoot, kbObject.getPathname());
                  parent = (DefaultMutableTreeNode)treeNode.getParent();
                  parent.remove(treeNode);
                  index = getObjectIndex(newName, parent);

                  // Always create fresh new objects to avoid bugs
                  parent.insert(new DefaultMutableTreeNode(new KBObject(kb, kbObject.getPathname()), isFolder), index);
                  treeModel.reload(parent);

                  // Ask if the user want to rename all instances of the object name
                  int replaceName =  JOptionPane.showConfirmDialog(app, "Replace all instances of this object name with the new name? (Requires closing all open objects)",
                  "Rename", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                  if (replaceName == JOptionPane.YES_OPTION)
                  {
                     // Close all the other object windows (for replace)
                     for (int i = 0 ; i < objectList.size() ; i++)
                     {
                        ((KBObjectFrame)objectList.elementAt(i)).okToClose(JOptionPane.YES_NO_OPTION);
                        ((KBObjectFrame)objectList.elementAt(i)).dispose();
                     }

                     // Now do the replace
                     statusFrame.replaceObjectName(kb, this, oldName, newName);
                  }
               }
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               return;
            }
         }
      }

      // Refresh the status frame
      if (app.getConfigEntry("auto_update_status").equals("true"))
         statusFrame.updateDisplay(null, null);
   }

/*   private void deleteObject()
   {
      DefaultTreeModel treeModel;
      DefaultMutableTreeNode treeNode;
      KBObject kbObject;
      String kbObjectName;

      DefaultMutableTreeNode node = getObjectSelection();
      if (node != null)
      {
         kbObjectName = node.toString();
         if (JOptionPane.showConfirmDialog(app, "OK to delete object: "+kbObjectName,
            res.getString("menuDelete.Text"), JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
         {
            try
            {
               kbObject = (KBObject)node.getUserObject();

               // Delete it from the KB
               if (kbObject.delete())
               {
                  // Then, delete it from both trees
                  treeModel = (DefaultTreeModel)kbObjectTree.getModel();
                  treeNode = findNode(treeModel, objectRoot, "/ " + kbObject.getType() + " / " + kbObject.getName());
                  treeModel.removeNodeFromParent(treeNode);
                  treeModel = (DefaultTreeModel)kbFolderTree.getModel();
                  treeNode = findNode(treeModel, folderRoot, kbObject.getPathname());
                  treeModel.removeNodeFromParent(treeNode);

                  // And close it if it's open
                  for (int i = 0 ; i < objectList.size() ; i++)
                  {
                     KBObjectFrame objFrame = ((KBObjectFrame)objectList.elementAt(i));
                     if (objFrame.getKBObject().getPathname() == kbObjectName)
                        objFrame.dispose();
                  }

               }
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               return;
            }
         }
      }
   }
*/
   // Delete multiple objects and folders
   private void deleteObjectsAndFolders()
   {
      DefaultMutableTreeNode node, newParent;
      DefaultTreeModel treeModel;
      TreePath selectedPaths[];
      Vector folders;
      KBObject kbObject;
      String treePathString, name, pathname, type;
      JTree tree;

      // Get selected items
      if (kbTabbedPane.getSelectedIndex() == 0)
      {
         selectedPaths = kbObjectTree.getSelectionPaths();
         tree = kbObjectTree;
      }
      else
      {
         selectedPaths = kbFolderTree.getSelectionPaths();
         tree = kbFolderTree;
      }

      // Make sure something is selected
      if (selectedPaths == null || selectedPaths.length == 0)
      {
         JOptionPane.showInternalMessageDialog(this,
            "Select one or more objects and folders to delete", "Delete",
            JOptionPane.INFORMATION_MESSAGE);
         return;
      }

      // Ask the user if its okay to delete them
      if (JOptionPane.showConfirmDialog(app, "OK to delete selected item(s)?",
         res.getString("menuDelete.Text"), JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
         return;

      // Walk the selected nodes, deleting each one
      for (int idx = 0 ; idx < selectedPaths.length ; idx++)
      {
         // Get the selected node
         treeModel = (DefaultTreeModel)tree.getModel();
         treePathString = objectPathToString(selectedPaths[idx]);
         if (tree == kbFolderTree)
            node = findNode(treeModel, folderRoot, treePathString);
         else
            node = findNode(treeModel, objectRoot, treePathString);

         // Delete it
         try
         {
            // Don't allow deletion of folders on the object tree
            if (tree == kbObjectTree && node.getAllowsChildren() == true)
               break;

            // Delete a folder
            if (node.getAllowsChildren() == true)
            {
               // Don't allow deletion of folders with objects in them
               if (node.getChildCount() > 0)
                  JOptionPane.showMessageDialog(app, "Folders must be empty before they can be deleted",
                     "ERROR", JOptionPane.ERROR_MESSAGE);

               // Delete it from the logicbase and tree
               if (kb.deleteFolder(objectPathToString(node)))
                  treeModel.removeNodeFromParent(node);
            }
            // Delete an object
            else
            {
               kbObject = (KBObject)node.getUserObject();
               name = kbObject.getName();
               pathname = kbObject.getPathname();
               type = kbObject.getType();

               // Delete it from both trees
               if (kbObject.delete())
               {
                  // Delete the node in the selected tree
                  treeModel.removeNodeFromParent(node);

                  // Then from the object tree
                  if (tree == kbFolderTree)
                  {
                     treeModel = (DefaultTreeModel)kbObjectTree.getModel();
                     node = findNode(treeModel, objectRoot, "/ "+type+" / "+name);
                     treeModel.removeNodeFromParent(node);
                  }
                  // Or the folder tree
                  else
                  {
                     treeModel = (DefaultTreeModel)kbFolderTree.getModel();
                     node = findNode(treeModel, folderRoot, pathname);
                     treeModel.removeNodeFromParent(node);
                  }

                  // And close it if it's open
                  for (int i = 0 ; i < objectList.size() ; i++)
                  {
                     KBObjectFrame objFrame = ((KBObjectFrame)objectList.elementAt(i));
                     if (objFrame.getKBObject().getPathname().equals(pathname))
                        objFrame.dispose();
                  }

               }
            }
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            return;
         }
      }

      // Refresh the status frame
      if (app.getConfigEntry("auto_update_status").equals("true"))
         statusFrame.updateDisplay(null, null);
   }

   // Move multiple objects and folders to another folder
   private void moveObjectsAndFolders()
   {
      DefaultMutableTreeNode node, newParent;
      DefaultTreeModel treeModel;
      TreePath selectedPaths[];
      Vector folders;
      KBObject kbObject;
      String treePathString;

      // Get selected items
      selectedPaths = kbFolderTree.getSelectionPaths();

      // Make sure folder tree is active and something is selected
      if (selectedPaths == null || kbTabbedPane.getSelectedIndex() != 1 || selectedPaths.length == 0)
      {
         JOptionPane.showInternalMessageDialog(this,
            "On the folder tree, select one or more objects and folders to move", "Move",
            JOptionPane.INFORMATION_MESSAGE);
         return;
      }

      // Ask the user where to move 'em
      try
      {
         // Get the folder to move it to
         folders = kb.getAllFolderPaths();
      }
      catch (KBUserException e)
      {
         JOptionPane.showMessageDialog(app, e.getMessage(),
            e.getErrorType(), JOptionPane.ERROR_MESSAGE);
         return;
      }

      Object[] possibleValues = new Object[folders.size()];
      for (int i = 0 ; i < folders.size() ; i++)
         possibleValues[i] = folders.elementAt(i);
      String prompt = "Move selected item(s) to folder:";
      Object newFolder = JOptionPane.showInputDialog(app, prompt, "Move",
         JOptionPane.QUESTION_MESSAGE, null, possibleValues, possibleValues[0]);

      // Forget it if the user cancelled
      if (newFolder == null) return;

      // Walk the selected nodes, moving each one
      for (int idx = 0 ; idx < selectedPaths.length ; idx++)
      {
         // Get the selected node
         treeModel = (DefaultTreeModel)kbFolderTree.getModel();
         treePathString = objectPathToString(selectedPaths[idx]);
         node = findNode(treeModel, folderRoot, treePathString);

         // And close it if it's open
         for (int i = 0 ; i < objectList.size() ; i++)
         {
            KBObjectFrame objFrame = ((KBObjectFrame)objectList.elementAt(i));
            if (objFrame.getKBObject().getPathname().equals(treePathString))
            {
               objFrame.okToClose(JOptionPane.YES_NO_OPTION);
               objFrame.dispose();
            }
         }

         // Move it
         try
         {
            // Move a folder
            if (node.getAllowsChildren() == true)
            {
               // Move it to the specified folder
               if (kb.moveFolder(objectPathToString(node), newFolder.toString()))
               {
                  // Remove it from the old folder
                  treeModel.removeNodeFromParent(node);

                  // Add it to the new folder
                  newParent = findNode(treeModel, folderRoot, newFolder.toString());
                  int index = getFolderIndex((String)node.getUserObject(), newParent);
                  treeModel.insertNodeInto(node, newParent, index);
               }
            }
            // Move an object
            else
            {
               kbObject = (KBObject)node.getUserObject();

               // Move it to the specified folder
               if (kbObject.move(newFolder.toString()))
               {
                  // Remove it from the old folder
                  treeModel.removeNodeFromParent(node);

                  // Add it to the new folder
                  newParent = findNode(treeModel, folderRoot, newFolder.toString());
                  int index = getObjectIndex(kbObject.getName(), newParent);
                  treeModel.insertNodeInto(node, newParent, index);
               }
            }
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            return;
         }
      } // for idx loop
   }

//----------------------------------------------------------------------------//
//                            Public Methods                                  //

   public void setTextFont(Font f)
   {
      kbFolderTree.setFont(f);
      kbObjectTree.setFont(f);
      repaint();
   }

   // Flag the KB as modified
   public void setKBNameModified(boolean setting)
   {
      if (setting)
         this.setTitle(kb.getName() + " *");
      else
         this.setTitle(kb.getName());
   }

   // Called when an KBObjectFrame is closed
   public void childFrameClosed(KBObjectFrame kbOF)
   {
      objectList.removeElement(kbOF);
   }

   public boolean okToClose(int option)
   {
      // Check if there are object windows open for this KB
      for (int i = 0 ; i < objectList.size() ; i++)
      {
         ((KBObjectFrame)objectList.elementAt(i)).okToClose(JOptionPane.YES_NO_OPTION);
         ((KBObjectFrame)objectList.elementAt(i)).dispose();
      }

      if (kb.isModified() == false)
      {
         try
         {
            kb.close();
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(app, ex.getMessage(),
               ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            return false;
         }
         dispose();
         app.removeOpenFile(kb.getPath());
         return true;
      }

      try
      {
         this.setSelected(true);
      }
      catch (Exception ex)
      {
      }
      this.show();

      // Confirm close w/o saving
      int value =  JOptionPane.showConfirmDialog(app, res.getString("SaveKBChanges.Prompt") +
         " " + kb.getName(), res.getString("menuClose.Text"), option) ;
      switch (value)
      {
         case JOptionPane.YES_OPTION:
            try
            {
               kb.save();
               kb.close();
               dispose();
               app.removeOpenFile(kb.getPath());
               return true;
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               return false;
            }

         case JOptionPane.NO_OPTION:
            try
            {
               kb.close();
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               return false;
            }
            dispose();
            app.removeOpenFile(kb.getPath());
            return true;

         case JOptionPane.CANCEL_OPTION:
         default:
            return false;
      }
   }


//----------------------------------------------------------------------------//
//                                 Utilities                                  //

   private DefaultMutableTreeNode getObjectSelection()
   {
      DefaultMutableTreeNode node;

      // Which tree is active
      if (kbTabbedPane.getSelectedIndex() == 0)
         node = (DefaultMutableTreeNode)
            kbObjectTree.getLastSelectedPathComponent();
      else
         node = (DefaultMutableTreeNode)
            kbFolderTree.getLastSelectedPathComponent();

      // If there is no selection boogie
      if (node == null) return null;

      // Folders aren't leaves, even if they have no children
      if (node.getAllowsChildren()) return null;

      return node;
   }

   private DefaultMutableTreeNode getFolderSelection()
   {
      DefaultMutableTreeNode node;

      // Which tree is active
      if (kbTabbedPane.getSelectedIndex() == 0)
         return null;
      else
         node = (DefaultMutableTreeNode)
            kbFolderTree.getLastSelectedPathComponent();

      if (node == null) return null;
      if (!node.getAllowsChildren()) return null;

      return node;
   }

   private String getSelectionObjectType() throws KBUserException
   {
      DefaultMutableTreeNode node;

      // Which tree is active
      if (kbTabbedPane.getSelectedIndex() == 0)
      {
         node = (DefaultMutableTreeNode)
            kbObjectTree.getLastSelectedPathComponent();
         if (node == null) return null;
         if (node.getAllowsChildren()) return node.getUserObject().toString();
         KBObject obj = (KBObject)node.getUserObject();
         return obj.getType();
      }
      else
      {
         node = (DefaultMutableTreeNode)
            kbFolderTree.getLastSelectedPathComponent();
         if (node == null) return null;
         if (node.getAllowsChildren()) return null;
         KBObject obj = (KBObject)node.getUserObject();
         return obj.getType();
      }
   }

   private String objectPathToString(DefaultMutableTreeNode node)
   {
      Object currentPathArray[] = node.getUserObjectPath();
      String currentPathString = "";
      for (int i = 0 ; i < currentPathArray.length ; i++)
         if (!currentPathArray[i].equals("/") && currentPathArray[i] != null)
            currentPathString = currentPathString + " / " + currentPathArray[i];
      return currentPathString.trim();
   }

   private String objectPathToString(TreePath treePath)
   {
      Object currentPathArray[] = treePath.getPath();
      String currentPathString = "";
      for (int i = 0 ; i < currentPathArray.length ; i++)
         if (!currentPathArray[i].toString().equals("/") && currentPathArray[i] != null)
            currentPathString = currentPathString + " / " + currentPathArray[i];
      return currentPathString.trim();
   }

   private DefaultMutableTreeNode findNode(DefaultTreeModel treeModel, DefaultMutableTreeNode tree, String pathName)
   {
      DefaultMutableTreeNode node;

      // Get the path in "standard" form
      pathName = Utils.quotePathname(pathName);

      // Return the root
      if (pathName.equals("/"))
         return (DefaultMutableTreeNode)tree.getRoot();

      node = (DefaultMutableTreeNode)tree.getFirstChild();
      while (node != null)
      {
         String currentPathString = Utils.quotePathname(objectPathToString(node));
         if (pathName.equals(currentPathString))
            return node;

         node = node.getNextNode();
      }

      return null;
   }

   private int getObjectIndex(String newName, DefaultMutableTreeNode parent)
   {
      int firstObject;

      // Put it at the top if there aren't any children
      if (parent.getChildCount() == 0) return 0;

      // Otherwise start at the first child
      DefaultMutableTreeNode node = (DefaultMutableTreeNode)parent.getFirstChild();

      // If there are no objects (just folders) add it at the beginning
      if (node.getAllowsChildren()) return 0;

      firstObject = parent.getIndex((TreeNode)node);
      while (node != null)
      {
         // If we get to the folders put it above the first folder
         if (node.getAllowsChildren()) return parent.getIndex((TreeNode)node);

         // Check the prior node
         int priorCompare = newName.compareTo(node.toString());
         if (priorCompare <= 0) return firstObject;

         // Then the next node (if it exists)
         DefaultMutableTreeNode nextNode = (DefaultMutableTreeNode)parent.getChildAfter(node);
         if (nextNode == null) return parent.getIndex((TreeNode)node) + 1;
         int nextCompare = newName.compareTo(nextNode.toString());

         // Compare the prior and the next
         if (priorCompare >= 0 && nextCompare <= 0)
            return parent.getIndex((TreeNode)node) + 1;

         // Continue searching the children
         node = nextNode;
      }
      return 0;
   }

   private int getFolderIndex(String newName, DefaultMutableTreeNode parent)
   {
      // No children, put it at the top
      if (parent.getChildCount() == 0) return 0;

      DefaultMutableTreeNode node = (DefaultMutableTreeNode)parent.getFirstChild();

      // Skip over all the objects
      while (node != null && !node.getAllowsChildren())
         node = (DefaultMutableTreeNode)parent.getChildAfter(node);

      // If there are no folders, add it at the end
      if (node == null)
         return parent.getIndex(parent.getLastChild()) + 1;

      while (node != null)
      {
         DefaultMutableTreeNode nextNode = (DefaultMutableTreeNode)parent.getChildAfter(node);

         // Put at the end of the folders
         if (nextNode == null) return parent.getIndex((TreeNode)node) + 1;
         int priorCompare = newName.compareToIgnoreCase(node.toString());
         int nextCompare = newName.compareToIgnoreCase(nextNode.toString());
         if (priorCompare <= 0) return parent.getIndex((TreeNode)node);
         if (priorCompare >= 0 && nextCompare <= 0)
            return parent.getIndex((TreeNode)node) + 1;
         node = nextNode;
      }
      return parent.getIndex(parent.getLastChild()) + 1;
   }


//----------------------------------------------------------------------------//
//                                Listeners                                   //

//----------------------------------------------------------------------------//

   // TreeSelectionListener

   // Listen for clicks on items in the tree
//   public void valueChanged(TreeSelectionEvent e)
//   {
//      editObject();
//   }

//----------------------------------------------------------------------------//

   // Listen for change in what tab pane is visible
   public void stateChanged(ChangeEvent e)
   {
      // Objects
      if (kbTabbedPane.getSelectedIndex() == 0)
      {
         menuFolder.setEnabled(false);
         menuObjectMove.setEnabled(false);
      }
      // Folders
      else
      {
         menuFolder.setEnabled(true);
         menuObjectMove.setEnabled(true);
      }
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

      // Delete
      if (e.getKeyCode() == KeyEvent.VK_DELETE)
         deleteObjectsAndFolders();

      try
      {
         // Help
         if (e.getKeyCode() == KeyEvent.VK_F1 || e.getKeyCode() == KeyEvent.VK_HELP)
         {
            String s = "file://" + docsDirectory + "jigs" + System.getProperty("file.separator")
               + kb.getHelpURL();
            Utils.displayURL(settings.get("web_browser"), s);
         }
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(app, "Unable to open web browser to display help:" +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
      }
   }

   public void keyTyped(KeyEvent e)
   {
   }

//----------------------------------------------------------------------------//

   // Mouse Listener

   public void mouseClicked(MouseEvent e)
   {
      TreePath selectedPaths[];

      if (kbTabbedPane.getSelectedIndex() == 0)
         selectedPaths = kbObjectTree.getSelectionPaths();
      else
         selectedPaths = kbFolderTree.getSelectionPaths();

      if (selectedPaths != null && e.getClickCount() == 2)
         if (selectedPaths.length == 1)
            editObject();
   }

   public void mouseEntered(MouseEvent e)
   {
   }

   public void mouseExited(MouseEvent e)
   {
   }

   public void mousePressed(MouseEvent e)
   {
      TreePath selectedPaths[];

      if (kbTabbedPane.getSelectedIndex() == 0)
         selectedPaths = kbObjectTree.getSelectionPaths();
      else
         selectedPaths = kbFolderTree.getSelectionPaths();

      if (selectedPaths != null && e.getClickCount() == 2)
         if (selectedPaths.length == 1)
            editObject();

       if (e.isPopupTrigger())
         popup.show(e.getComponent(), e.getX(), e.getY());
   }

   public void mouseReleased(MouseEvent e)
   {
      TreePath selectedPaths[];

      if (kbTabbedPane.getSelectedIndex() == 0)
         selectedPaths = kbObjectTree.getSelectionPaths();
      else
         selectedPaths = kbFolderTree.getSelectionPaths();

      if (selectedPaths != null && e.getClickCount() == 2)
         if (selectedPaths.length == 1)
            editObject();

      if (e.isPopupTrigger())
         popup.show(e.getComponent(), e.getX(), e.getY());
    }


//----------------------------------------------------------------------------//
//                          Supporting Classes                                //

   class kbTreeRenderer extends DefaultTreeCellRenderer {

      public kbTreeRenderer()
      {
      }

      public Component getTreeCellRendererComponent(
         JTree tree,
         Object value,
         boolean sel,
         boolean expanded,
         boolean leaf,
         int row,
         boolean hasFocus)
      {
         KBObject kbObject;

         super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
            row, hasFocus);
         if (leaf)
         {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;
            kbObject = (KBObject)node.getUserObject();
            String type = kbObject.getType();
            if (type.equals("folder"))
            {
               setToolTipText(null);
            }
            else
            {
               try
               {
                  setIcon(kbObject.getIcon());
                  setToolTipText(kbObject.getObjectHelp());
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(app, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
               }
            }
         }
         else
         {
            setToolTipText(null); //no tool tip
         }

         return this;
      }
   }

}
