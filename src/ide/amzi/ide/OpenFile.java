
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:
package amzi.ide;

import javax.swing.*;

public class OpenFile
{
   private String path;
   private JInternalFrame frame;
   private KB kb;

   public OpenFile(String path, JInternalFrame frame)
   {
      this.path = path;
      this.frame = frame;
      this.kb = null;
   }

   public OpenFile(String path, JInternalFrame frame, KB kb)
   {
      this.path = path;
      this.frame = frame;
      this.kb = kb;
   }

   public String getPath()
   {
      return path;
   }

   public JInternalFrame getFrame()
   {
      return frame;
   }

   public KB getKB()
   {
      return kb;
   }

   public void setPath(String path)
   {
      this.path = path;
   }
}