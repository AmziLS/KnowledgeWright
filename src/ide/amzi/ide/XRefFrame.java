/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class XRefFrame extends JPanel
   implements ItemListener, ListSelectionListener
{
   JPanel listPanel = new JPanel();
   JPanel leftPanel = new JPanel();
   JPanel rightPanel = new JPanel();
   BorderLayout leftBorderLayout = new BorderLayout();
   BorderLayout rightBorderLayout = new BorderLayout();
   GridLayout mainGridLayout = new GridLayout(1,3,5,5);
   BorderLayout listBorderLayout = new BorderLayout();
   JComboBox objectTypes;
   JList objectList;
   JScrollPane objectListScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
   JTextArea usedBy = new JTextArea();
   JScrollPane usedByScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
   JTextArea uses = new JTextArea();
   JScrollPane usesScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
   App app;
   KB kb;

   public XRefFrame(App app, KB kb)
   {
      super();
      this.app = app;
      this.kb = kb;
      try
      {
         jbInit();
      }
      catch(Exception ex) {
         ex.printStackTrace();
      }
   }

   void jbInit() throws Exception
   {
      // List panel
      objectTypes = new JComboBox(kb.getObjectTypes());
      objectTypes.setFont(app.getUserFont());
//      objectTypes.setSelectedIndex(0);
      objectTypes.setEditable(false);
      objectTypes.addItemListener(this);
      objectList = new JList(kb.getObjectPaths((String)objectTypes.getSelectedItem()));
      objectList.setFont(app.getUserFont());
      objectList.addListSelectionListener(this);
//      objectList.setSelectedIndex(0);
      listPanel.setLayout(listBorderLayout);
      listPanel.add(objectTypes, BorderLayout.NORTH);
      objectListScroller.getViewport().add(objectList, null);
      objectListScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      listPanel.add(objectListScroller, BorderLayout.CENTER);

      // Left panel
//      usedBy.setColumns(25);
//      usedBy.setRows(10);
      usedBy.setFont(app.getUserFont());
      usedBy.setEditable(false);
      leftPanel.setLayout(leftBorderLayout);
      leftPanel.add(new JLabel("Object is used by:"), BorderLayout.NORTH);
      usedByScroller.getViewport().add(usedBy, null);
      leftPanel.add(usedByScroller, BorderLayout.CENTER);

      // Right panel
//      uses.setColumns(25);
//      uses.setRows(10);
      uses.setFont(app.getUserFont());
      uses.setEditable(false);
      rightPanel.setLayout(rightBorderLayout);
      rightPanel.add(new JLabel("Object uses:"), BorderLayout.NORTH);
      usesScroller.getViewport().add(uses, null);
      rightPanel.add(usesScroller, BorderLayout.CENTER);

      // Main panel
      this.setLayout(mainGridLayout);
      this.add(leftPanel);
      this.add(listPanel);
      this.add(rightPanel);
   }

   public void select(String objectName, String pathname)
   {
      objectTypes.setSelectedItem((Object)objectName);
      objectList.setSelectedValue((Object)pathname, true);
      uses.setCaretPosition(0);
      usedBy.setCaretPosition(0);
   }

   public void setTextFont(Font f)
   {
      uses.setFont(f);
      usedBy.setFont(f);
      objectTypes.setFont(f);
      objectList.setFont(f);
      repaint();
   }

//----------------------------------------------------------------------------//
//                                  Listeners                                 //

   // ItemListener
   public void itemStateChanged(ItemEvent e)
   {
      try
      {
         objectList.setListData(kb.getObjectPaths((String)e.getItem()));
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // ListSelectionListener
   public void valueChanged(ListSelectionEvent e)
   {
      try
      {
         if (e.getValueIsAdjusting() == false)
         {
//            JList list = (JList)e.getSource();
//            String s = (String)list.getSelectedValue();
            if (((JList)e.getSource()).getSelectedValue() != null)
            {
               KBObject kbObject = new KBObject(kb, (String)((JList)e.getSource()).getSelectedValue());
               uses.setText(kbObject.getUses());
               usedBy.setText(kbObject.getUsedBy());
            }
         }
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

}