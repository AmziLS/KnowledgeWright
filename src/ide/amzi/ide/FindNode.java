/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import javax.swing.*;

public class FindNode
{
   String pathname, slot, slotValue, findText, locType, iconName;
   int row, col, index;

   public FindNode(String pathname, String slot, String slotValue, String findText,
      String locType, int row, int col, int index, String iconName)
   {
      this.iconName = iconName;
      this.pathname = pathname;
      this.slot = slot;
      this.slotValue = slotValue;
      this.findText = findText;
      this.locType = locType;
      this.row = row;
      this.col = col;
      this.index = index;
   }

   public ImageIcon getIcon()
   {
      if (getClass().getResource("resources/"+iconName) != null)
         return new ImageIcon((getClass().getResource("resources/"+iconName)));
      else
         return new ImageIcon((getClass().getResource("resources/default_icon.gif")));
   }

   public String getPathname()
   {
      return pathname;
   }

   public String toString()
   {
      int start, end;

      if (index < 100) start = 0;
      else start = slotValue.indexOf(' ', index-100);
      if (start == -1) start = 0;
      if (index + 100 > slotValue.length() || index == -1) end = slotValue.length();
      else end = slotValue.indexOf(' ',  index+100);
      if (end == -1) end = slotValue.length();

      char lineSep[] = "\n".toCharArray();
      return slot + ": " + slotValue.substring(start, end).replace(lineSep[0], ' ');
   }
}