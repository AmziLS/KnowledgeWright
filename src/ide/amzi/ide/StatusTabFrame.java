package amzi.ide;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyVetoException;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

public class StatusTabFrame extends JInternalFrame
{
   JTabbedPane tabPane = new JTabbedPane();
   XRefFrame xref;
   IntegrityCheckerFrame checker;
   FindResults finder;
   ReplaceResults replacer;
   App app;
   KB kb;

   public StatusTabFrame(App app, KB kb)
   {
      // Set title, resizable, not closable, maximizable, iconifiable
      super("Knowledgebase Status", true, false, true, true);
      try
      {
         this.app = app;
         this.kb = kb;
         xref = new XRefFrame(app, kb);
         checker = new IntegrityCheckerFrame(app, kb, false);
         finder = new FindResults(app, kb);
         replacer = new ReplaceResults(app, kb);

         jbInit();
      }
      catch(Exception e) {
         e.printStackTrace();
      }
   }

   private void jbInit() throws Exception
   {
      this.setVisible(true);
      ImageIcon icon = kb.getIcon();
      this.setFrameIcon(icon);

      tabPane.setTabPlacement(JTabbedPane.LEFT);
      this.getContentPane().add(tabPane, BorderLayout.CENTER);

      // If the order of the tabs is changed, check public methods
      tabPane.addTab(null, new ImageIcon(getClass().getResource("resources/exclaim.gif")), checker, "Problems in the knowledgebase");
      tabPane.addTab(null, new ImageIcon(getClass().getResource("resources/web.gif")), xref, "Object inter-relationships");
      tabPane.addTab(null, new ImageIcon(getClass().getResource("resources/find.gif")), finder, "Search results");
      tabPane.addTab(null, new ImageIcon(getClass().getResource("resources/replace.gif")), replacer, "Replace results");
//      tabPane.addChangeListener(this);
   }

   public void updateDisplay(String objectType, String pathname)
   {
      checker.recheck();
      if (objectType != null && pathname != null)
         xref.select(objectType, pathname);
   }

   public void findText(KB kb, KBTreeFrame kbTreeFrame, String text, boolean matchcase)
   {
      try
      {
         if (this.isIcon) this.setIcon(false);
      }
      catch (PropertyVetoException ex)
      {
         this.requestFocus();
      }

      finder.findText(kb, kbTreeFrame, text, matchcase);
      tabPane.setSelectedIndex(2);
   }

   public void replaceObjectName(KB kb, KBTreeFrame kbTreeFrame, String oldName, String newName)
   {
      try
      {
         if (this.isIcon) this.setIcon(false);
      }
      catch (PropertyVetoException ex)
      {
         this.requestFocus();
      }

      replacer.replaceObjectName(kb, kbTreeFrame, oldName, newName);
      tabPane.setSelectedIndex(3);
   }

   public void setTextFont(Font f)
   {
      xref.setTextFont(f);
      checker.setTextFont(f);
      replacer.setTextFont(f);
      finder.setTextFont(f);
   }
}