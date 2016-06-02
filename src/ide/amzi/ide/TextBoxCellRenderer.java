/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import javax.swing.table.TableCellRenderer;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FlowLayout;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

public class TextBoxCellRenderer extends JScrollPane implements TableCellRenderer
{
   private JTextArea textBox;

   public TextBoxCellRenderer(Font font)
   {
      super(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      textBox = new JTextArea();
      textBox.setFont(font);
      textBox.setWrapStyleWord(true);
      textBox.setLineWrap(true);
      this.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      this.getViewport().add(textBox, null);
   }

   public Component getTableCellRendererComponent(JTable table, Object text,
      boolean isSelected, boolean hasFocus, int row, int column)
   {
      textBox.setText((String)text);
      return this;
   }
}