
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import java.util.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

public class SavedSettings
{
   private Properties props;

   public SavedSettings() throws IOException
   {
      String iniPath = "";

      // Set INI default values
      Properties defaults = new Properties();
      defaults.put("web_browser", "");
      defaults.put("run_log_detail", "medium");
      defaults.put("auto_update_status", "true");
      defaults.put("unicode", "false");
      defaults.put("screen_x", "0");
      defaults.put("screen_y", "0");
      defaults.put("screen_w", "0");
      defaults.put("screen_h", "0");
      defaults.put("tree_x", "0");
      defaults.put("tree_y", "0");
      defaults.put("tree_w", "0");
      defaults.put("tree_h", "0");
      defaults.put("status_x", "0");
      defaults.put("status_y", "0");
      defaults.put("status_w", "0");
      defaults.put("status_h", "0");
      defaults.put("run_log_x", "0");
      defaults.put("run_log_y", "0");
      defaults.put("run_log_w", "0");
      defaults.put("run_log_h", "0");
      defaults.put("run_out_x", "0");
      defaults.put("run_out_y", "0");
      defaults.put("run_out_w", "0");
      defaults.put("run_out_h", "0");
      defaults.put("font_name", "");
      defaults.put("font_size", "0");
      defaults.put("font_style", "0");
      defaults.put("user_name", "");
      defaults.put("os_name", "");
      defaults.put("os_version", "");
      defaults.put("kb_open", "false");
      defaults.put("license_name", "");
      defaults.put("license_organization", "");
      defaults.put("serial_number", "");
      defaults.put("unlock_code", "");
      defaults.put("license_type", "personal");
      defaults.put("maintenance_reminder", "1");
      defaults.put("locale", "");
      for (int i = 0 ; i < App.REOPEN_LIST_SIZE ; i++)
         defaults.put("open"+new Integer(i).toString(), "");
      props = new Properties(defaults);

      try
      {
         iniPath = Utils.findClasspathFile(App.CONFIG_FILE, "");
         File inif = new File(iniPath);
         if (!inif.exists())
         {
            JOptionPane.showMessageDialog(null, "Unable to find kw.cfg file at: " + iniPath + " - a new one will be created",
               "WARNING", JOptionPane.WARNING_MESSAGE);
//            save();
         }
         BufferedReader in = new BufferedReader(new FileReader(inif));
         String line = in.readLine();
         while (line != null)
         {
            String attr = null;
            String val = null;
            StringTokenizer tokens = new StringTokenizer(line, "=");
            if (tokens.hasMoreTokens())
            {
               attr = tokens.nextToken().trim();
               if (tokens.hasMoreTokens())
               {
                  val = tokens.nextToken().trim();
                  props.put(attr, val);
               }
            }
            line = in.readLine();
         }
         in.close();
      }
      catch (FileNotFoundException ex)
      {
            JOptionPane.showMessageDialog(null, "Unable to find kw.cfg file at: " + iniPath + " - " + ex.getMessage(),
               "ERROR", JOptionPane.ERROR_MESSAGE);
      }
   }

   public String get(String attr)
   {
      return (String)props.getProperty(attr);
   }

   public void set(String attr, String value)
   {
      props.setProperty(attr, value);
   }

   public void save() throws IOException
   {
      File iniFile;

      try
      {
         String iniPath = Utils.findClasspathFile(App.CONFIG_FILE, "");
         if (iniPath != null)
            iniFile = new File(iniPath);
         else
            iniFile = new File(System.getProperty("user.dir")+System.getProperty("file.separator")+App.CONFIG_FILE);
         BufferedWriter out = new BufferedWriter(new FileWriter(iniFile));
         Enumeration propNames = props.propertyNames();
         while (propNames.hasMoreElements())
         {
            String name = (String)propNames.nextElement();
            out.write(name + " = " + props.getProperty(name));
            out.newLine();
         }
         out.close();
      }
      catch (FileNotFoundException ex)
      {
//         System.out.println("Can't find ide.cfg");
      }
   }
}
