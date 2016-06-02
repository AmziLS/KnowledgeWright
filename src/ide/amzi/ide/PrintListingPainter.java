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
import java.awt.print.*;
import java.io.*;
import java.lang.String;

class PrintListingPainter implements Printable
{
   private RandomAccessFile raf;
   private String fileName;
   private Font font;
   private int rememberedPageIndex = -1;
   private long rememberedFilePointer = -1;
   private boolean rememberedEOF = false;
   private String leftOver = "";
   private int leftOverPage = -1;

   public PrintListingPainter(String file, Font font)
   {
      fileName = file;
      this.font = font;
      leftOver = "";
      try
      {
         // Open file
         raf = new RandomAccessFile(file, "r");
      }
      catch (Exception e)
      { rememberedEOF = true; }
   }

   public int print(Graphics g, PageFormat pf, int pageIndex)
   throws PrinterException
   {
      String line;
      boolean didLeftOver = false;

      g.setColor(Color.black);
      g.setFont(font);

      FontMetrics fm = g.getFontMetrics();
      int lineInc = fm.getHeight();

      try
      {
         // For catching IOException
         if (pageIndex != rememberedPageIndex)
         {
            // First time we've visited this page
            rememberedPageIndex = pageIndex;

            // If encountered EOF on previous page, done
            if (rememberedEOF) return Printable.NO_SUCH_PAGE;

            // Save current position in input file
            rememberedFilePointer = raf.getFilePointer();
         }
         else
            raf.seek(rememberedFilePointer);

         int x = (int) pf.getImageableX();
         int y = (int) pf.getImageableY() + lineInc;

         // Title line
         g.drawString(Integer.toString(pageIndex+1) + "  --   " + fileName, x, y);
         y += lineInc*2;

         // Generate as many lines as will fit in imageable area
         while (y + lineInc < pf.getImageableY()+pf.getImageableHeight())
         {
            // If something was left over and we're on a new page, write it first
            if (leftOver.length() > 0 && pageIndex == leftOverPage && didLeftOver == false)
            {
               line = leftOver;
               didLeftOver = true;
            }
            else
               line = readUnicodeLine(raf);
            if (line == null)
            {
               rememberedEOF = true;
               break;
            }
            // If the line is too long, split it
            while (fm.stringWidth(line) > pf.getImageableWidth() && y + lineInc < pf.getImageableY()+pf.getImageableHeight())
            {
               int split = new Double(line.length() * (pf.getImageableWidth() / fm.stringWidth(line)) - 1).intValue();
               while (split > 0 && !Character.isWhitespace(line.charAt(split))) split = split - 1;
               if (split == 0) split = new Double(line.length() * (pf.getImageableWidth() / fm.stringWidth(line)) - 1).intValue();
               g.drawString(line.substring(0, split), x, y);
               y += lineInc;
               line = line.substring(split+1);
            }

            // If we ran out of space on the page, save the leftover text for the next page
            if (y + lineInc < pf.getImageableY()+pf.getImageableHeight())
            {
               g.drawString(line, x, y);
               y += lineInc;
            }
            else
            {
               leftOver = line;
               leftOverPage = pageIndex + 1;
               break;
            }
         }

         return Printable.PAGE_EXISTS;
      }
      catch (Exception ex)
      {
         return Printable.NO_SUCH_PAGE;
      }
   }

   private String readUnicodeLine(RandomAccessFile raf)
   {
      String s = null;
      byte b1, b2;
      char c;
      long eol;

      try
      {
         do
         {
            b1 = raf.readByte();
            b2 = raf.readByte();
            c = (char) (b2 << 8 | b1);
         } while (b1 == -1 && b2 == -2);
         s = "";

//         if (b1 == -1 && b2 == -2) bigEndian = false;

         while (c != '\n')
         {
            if (!Character.isISOControl(c)) s = s + String.valueOf(c);
            b1 = raf.readByte();
            b2 = raf.readByte();
            c = (char) (b2 << 8 | b1);
         }
      }
      catch (IOException ex)
      {
         s = null;
      }

      if (s != null)
         if (s.equals("\n")) s = "";
      return s;
   }

}
