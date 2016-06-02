package amzi.ide;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

public class TextBoxCellEditor extends AbstractCellEditor implements TableCellEditor
{
   private JTextArea textBox;
   private JScrollPane textScroller;

   public TextBoxCellEditor(JTextArea textBoxIn, Font font)
   {
      super();
      textBox = textBoxIn;
      textBox.setWrapStyleWord(true);
      textBox.setLineWrap(true);
      textBox.setFont(font);

      textScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      textScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      textScroller.getViewport().add(textBox, null);
   }

   public Object getCellEditorValue()
   {
      return (Object)textBox.getText();
   }

   public Component getTableCellEditorComponent(JTable table, Object text,
      boolean isSelected, int row, int column)
   {
      textBox.setText((String)text);
      Dimension d = textBox.getPreferredSize();
      int extra = textBox.getFontMetrics(textBox.getFont()).getHeight()*3;
      if (table.getRowHeight(row) < d.height+extra)
      {
         table.setRowHeight(row, d.height+extra);
         table.setPreferredScrollableViewportSize(table.getPreferredSize());
      }

/*      textBox.validate();

      int lineHeight = textBox.getFontMetrics(textBox.getFont()).getHeight();
      int lineCount = textBox.getLineCount();

      // Change here and in KBObjectFrame/TableListener/ValueChanged (NO)
      if (lineCount < minRows) lineCount = minRows;
      if (table != null)
      {
         table.setRowHeight(row, lineHeight * (lineCount + 1));
         table.validate();
      }
*/
      return textScroller;
   }

   public JTextArea getTextArea()
   {
      return textBox;
   }

}