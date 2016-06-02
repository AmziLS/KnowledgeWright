/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2002 Amzi! inc. All Rights Reserved.
 * Company:
 * @author
 * @version 1.0
 */

import java.io.*;

public class ExtFilenameFilter implements FilenameFilter
{
   String ext;

   public ExtFilenameFilter(String ext)
   {
      this.ext = ext;
   }

   public boolean accept(File dir, String name)
   {
	   int i = name.lastIndexOf('.');
	   if(i > 0 && i < name.length()-1)
      if (name.substring(i+1).toLowerCase().equals(ext)) return true;
      return false;
   }
}