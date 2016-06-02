/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import amzi.ls.*;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

public class ReplaceResults extends JPanel implements TreeSelectionListener
{
   JScrollPane replaceScroller = new JScrollPane();
   JTree replacedTree = null;
   App app;
   KB kb;
   KBTreeFrame kbTreeFrame;

   private DefaultMutableTreeNode foundRoot;

   public ReplaceResults(App app, KB kb)
   {
      super();
      this.app = app;
      try
      {
         this.setLayout(new BorderLayout());
         this.add(replaceScroller, BorderLayout.CENTER);
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public void replaceObjectName(KB kb, KBTreeFrame kbTreeFrame, String oldName, String newName)
   {
      this.kb = kb;
      this.kbTreeFrame = kbTreeFrame;

      try
      {
         replaceScroller.getViewport().removeAll();
         if (oldName != null && newName != null)
         {
            long list = kb.replaceObjectName(oldName, newName);
            buildResultsTree(kb, oldName, newName, list);
            replaceScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
            replaceScroller.getViewport().add(replacedTree, null);
         }
      }
      catch (KBUserException ex)
      {
         System.out.println(ex);
      }
   }

   private void buildResultsTree(KB kb, String oldName, String newName, long list)
   {
      LogicServer ls;
      long item, loc;
      String path, name, pathname, slot, string, iconName, rootString,
         lastPathname = "";
      FindNode node;
      DefaultMutableTreeNode foundRoot, objectFolder = null;

      // Get a logicserver
      ls = kb.getLogicServer();

      // Make a root for the tree and a place to hang the results off of
      rootString = "Replace Results for \"" + oldName + "\" to \"" + newName + "\"";
      foundRoot = new DefaultMutableTreeNode(rootString, true);

      do
      {
         try
         {
            // Check for the empty list or an atom
            if (list == 0 || ls.GetTermType(list) != LogicServer.pLIST) break;

            // Otherwise get the head
            // Returns found(path, name, slot, newvalue, icon)
            item = ls.GetHead(list);
            path = ls.TermToStr(ls.GetArg(item, 1), 10000);
            name = ls.GetStrArg(item, 2);
            if (path.equals("/")) pathname = "/ " + name;
            else pathname = path + " / " + name;
            slot = ls.GetStrArg(item, 3);
            string = ls.TermToStr(ls.GetArg(item, 4), 100000);
            iconName = ls.GetStrArg(item, 5);

            // Create a node for the tree
            node = new FindNode(pathname, slot, string, newName, "none",
               -1, -1, -1, iconName);

            // If we have a new object create a folder
            if (!pathname.equals(lastPathname))
            {
               lastPathname = pathname;
               objectFolder = new DefaultMutableTreeNode(pathname, true);
               foundRoot.add(objectFolder);
            }

            // Add this find instance to the current object folder
            objectFolder.add(new DefaultMutableTreeNode(node, false));

            list = ls.GetTail(list);
         }
         catch (LSException ex)
         {
//            throw new KBUserException("auth_find list dissection", ex);
            System.out.println(ex);
         }
      }
      while (list != 0);

      // Set up the tree finally (must be done after the nodes are created)
      replacedTree = new JTree(foundRoot, true);
      replacedTree.setFont(app.getUserFont());
      replacedTree.setRootVisible(true);
      replacedTree.setShowsRootHandles(true);
      replacedTree.putClientProperty("JTree.lineStyle", "Horizontal");   // Horizontal or None
      replacedTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

      // Change how the tree is rendered
      replacedTree.setCellRenderer(new FoundTreeCellRenderer());

      // Expand all the paths
      int lastRow = replacedTree.getRowCount();
      for (int i = lastRow ; i >= 0 ; i--)
         replacedTree.expandRow(i);

     replacedTree.addTreeSelectionListener(this);
   }

   public void valueChanged(TreeSelectionEvent e)
   {
      // Get the selected node
      DefaultMutableTreeNode node = (DefaultMutableTreeNode)replacedTree.getLastSelectedPathComponent();

      // Do nothing if its not a leaf
      if (node.getAllowsChildren()) return;

      // Otherwise get the info and open the object
      FindNode findNode = (FindNode)node.getUserObject();
      try
      {
         kbTreeFrame.editObjectFrame(new KBObject(kb, findNode.getPathname()));
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
         return;
      }
   }

   public void setTextFont(Font f)
   {
      if (replacedTree != null)
      {
         replacedTree.setFont(f);
         this.repaint();
      }
   }

//----------------------------------------------------------------------------//
//                          Supporting Classes                                //

   class FoundTreeCellRenderer extends DefaultTreeCellRenderer {

      public FoundTreeCellRenderer()
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
         super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
            row, hasFocus);
         if (leaf)
         {
            setIcon(null);
         }
         else
         {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;

            if (node.isRoot())
               setIcon(null);
            else
            {
               DefaultMutableTreeNode childNode = (DefaultMutableTreeNode)node.getFirstChild();
               FindNode findNode = (FindNode)childNode.getUserObject();
               setIcon(findNode.getIcon());
            }
         }

         return this;
      }
   }
}