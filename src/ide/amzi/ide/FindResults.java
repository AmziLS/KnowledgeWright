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

public class FindResults extends JPanel implements TreeSelectionListener
{
   JScrollPane foundScroller = new JScrollPane();
   JTree foundTree = null;
   App app;
   KB kb;
   KBTreeFrame kbTreeFrame;

   private DefaultMutableTreeNode foundRoot;

   public FindResults(App app, KB kb)
   {
      super();
      this.app = app;
      try
      {
         this.setLayout(new BorderLayout());
         this.add(foundScroller, BorderLayout.CENTER);
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }

   public void findText(KB kb, KBTreeFrame kbTreeFrame, String text, boolean matchcase)
   {
      this.kb = kb;
      this.kbTreeFrame = kbTreeFrame;

      try
      {
         foundScroller.getViewport().removeAll();
         if (text != null)
         {
            long list = kb.findText(text, matchcase);
            buildResultsTree(kb, text, list);
            foundScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
            foundScroller.getViewport().add(foundTree, null);
         }
      }
      catch (KBUserException ex)
      {
         System.out.println(ex);
      }
   }

   private void buildResultsTree(KB kb, String text, long list)
   {
      LogicServer ls;
      long item, loc;
      String path, name, pathname, slot, string, locType, iconName, rootString,
         lastPathname = "";
      int row = -1, col = -1, index = -1;
      FindNode node;
      DefaultMutableTreeNode foundRoot, objectFolder = null;

      // Get a logicserver
      ls = kb.getLogicServer();

      // Make a root for the tree and a place to hang the results off of
      rootString = "Search Results for \"" + text + "\"";
      foundRoot = new DefaultMutableTreeNode(rootString, true);

      do
      {
         try
         {
            // Check for the empty list or an atom
            if (list == 0 || ls.GetTermType(list) != LogicServer.pLIST) break;

            // Otherwise get the head
            // Returns found(path, name, slot, string, loc, icon)
            // loc is index(I) or list(row,index) or table(row,col,index)
            item = ls.GetHead(list);
            path = ls.TermToStr(ls.GetArg(item, 1), 10000);
            name = ls.GetStrArg(item, 2);
            if (path.equals("/")) pathname = "/ " + name;
            else pathname = path + " / " + name;
            slot = ls.GetStrArg(item, 3);
            string = ls.GetStrArg(item, 4);
            loc = ls.GetArg(item, 5);
            iconName = ls.GetStrArg(item, 6);
            locType = ls.GetFunctor(loc);
            if (locType.equals("index"))
            {
               row = -1;
               col = -1;
               index = ls.GetIntArg(loc, 1);
            }
            if (locType.equals("list"))
            {
               row = ls.GetIntArg(loc, 1);
               index = ls.GetIntArg(loc, 2);
               col = -1;
            }
            if (locType.equals("table"))
            {
               row = ls.GetIntArg(loc, 1);
               col = ls.GetIntArg(loc, 2);
               index = ls.GetIntArg(loc, 3);
            }

            // Create a node for the tree
            node = new FindNode(pathname, slot, string, text, locType,
               row, col, index, iconName);

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
      foundTree = new JTree(foundRoot, true);
      foundTree.setFont(app.getUserFont());
      foundTree.setRootVisible(true);
      foundTree.setShowsRootHandles(true);
      foundTree.putClientProperty("JTree.lineStyle", "Horizontal");   // Horizontal or None
      foundTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

      // Change how the tree is rendered
      foundTree.setCellRenderer(new FoundTreeCellRenderer());

      // Expand all the paths
      int lastRow = foundTree.getRowCount();
      for (int i = lastRow ; i >= 0 ; i--)
         foundTree.expandRow(i);

     foundTree.addTreeSelectionListener(this);
   }

   public void valueChanged(TreeSelectionEvent e)
   {
      // Get the selected node
      DefaultMutableTreeNode node = (DefaultMutableTreeNode)foundTree.getLastSelectedPathComponent();

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
      if (foundTree != null)
      {
         foundTree.setFont(f);
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