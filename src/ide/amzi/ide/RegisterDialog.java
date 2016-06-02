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
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

public class RegisterDialog extends JDialog
{
   private ResourceBundle res = ResourceBundle.getBundle("amzi.ide.resources.IDE");
   BorderLayout borderLayout = new BorderLayout();
   JPanel buttonPanel = new JPanel();
   FlowLayout buttonFlowLayout = new FlowLayout();
   JButton okButton = new JButton();
   JButton cancelButton = new JButton();
   JPanel mainPanel = new JPanel();
   GridBagLayout gridBagLayout = new GridBagLayout();
   JLabel nameLabel = new JLabel();
   JTextField nameField = new JTextField();
   JLabel orgLabel = new JLabel();
   JTextField orgField = new JTextField();
   JLabel serialLabel = new JLabel();
   JTextField serialField = new JTextField();
   JLabel unlockLabel = new JLabel();
   JTextField unlockField = new JTextField();
   private String licenseType;

   App app;
   SavedSettings settings;
   String editXPL, runtimeDir;

   public RegisterDialog(App app, String title, String editXPL, String runtimeDir, SavedSettings settings)
   {
      super(app, title, true /*modal*/);
      try
      {
         this.app = app;
         this.editXPL = editXPL;
         this.runtimeDir = runtimeDir;
         this.settings = settings;

         nameField.setText(settings.get("license_name"));
         orgField.setText(settings.get("license_organization"));
         serialField.setText(settings.get("serial_number"));
         unlockField.setText(settings.get("unlock_code"));

         jbInit();
         pack();
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
   }
   private void jbInit() throws Exception
   {
      this.getContentPane().setLayout(borderLayout);

      this.getRootPane().setDefaultButton(okButton);
      okButton.setText("OK");
      okButton.setMnemonic(KeyEvent.VK_ENTER);
      okButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            okButton_actionPerformed(e);
         }
      });

      cancelButton.setMnemonic(KeyEvent.VK_ESCAPE);
      cancelButton.setText("Cancel");
      cancelButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            cancelButton_actionPerformed(e);
         }
      });

      nameLabel.setText("Name: ");
      nameField.setColumns(30);
      orgLabel.setText("Organization: ");
      orgField.setColumns(30);
      serialLabel.setText("Serial Number: ");
      serialField.setColumns(23);
      unlockLabel.setText("Unlock Code: ");
      unlockField.setColumns(19);

      mainPanel.setLayout(gridBagLayout);
      mainPanel.add(nameLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(nameField,  new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(orgLabel,  new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(orgField,  new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(serialLabel,  new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(serialField,  new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(unlockLabel,  new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      mainPanel.add(unlockField,  new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
      this.getContentPane().add(mainPanel, BorderLayout.CENTER);

      // Must add this last so that the nameField gets the focus!
      buttonPanel.setLayout(buttonFlowLayout);
      this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
      buttonPanel.add(okButton, null);
      buttonPanel.add(cancelButton, null);
   }

   void okButton_actionPerformed(ActionEvent e)
   {
      String s;

      // Check formatting
      if (nameField.getText().length() > 30 || orgField.getText().length() > 30)
      {
         JOptionPane.showMessageDialog(app, "Name and Organization must be 30 characters or less",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         return;
      }
      s = serialField.getText();
      if (s.length() != 23 || s.charAt(5) != '-'  || s.charAt(11) != '-' || s.charAt(17) != '-')
      {
         JOptionPane.showMessageDialog(app, "Serial number must be of the form XXXXX-XXXXX-XXXXX-XXXXX",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         return;
      }
      s = unlockField.getText();
      if (s.length() != 19 || s.charAt(4) != '-'  || s.charAt(9) != '-' || s.charAt(14) != '-')
      {
         JOptionPane.showMessageDialog(app, "Unlock code must be of the form XXXX-XXXX-XXXX-XXXX",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         return;
      }
      String problem = register(nameField.getText(), orgField.getText(), serialField.getText(), unlockField.getText());
      if (problem == null)
      {
         JOptionPane.showMessageDialog(app, "Your license has been successfully entered",
            "Success", JOptionPane.INFORMATION_MESSAGE);
         saveDetails(licenseType);
         dispose();
      }
      else
         JOptionPane.showMessageDialog(app, problem,
            "ERROR", JOptionPane.ERROR_MESSAGE);
   }

   void cancelButton_actionPerformed(ActionEvent e)
   {
      saveDetails("personal");
      dispose();
   }

   public String register(String name, String org, String serial, String unlock)
   {
      long term;
      String s, xplPath;
      LogicServer ls = null;
      String problem = null;

      try
      {
         ls = new LogicServer();
         ls.Init("");
         xplPath = Utils.findClasspathFile(editXPL, "");
         ls.Load(xplPath);

         // First see if this is a product id that matches this product
         s = "register:unl$pid($" + serial + "$, _Product, _Platform, _Version, _Users, _SerialNum, _XMonth, _XMonthName, _XDay, _XYear)";
         term = ls.ExecStr(s);
         if (term == 0) problem = "Invalid registration details. Please try again.";
         else
         {
            String product = ls.GetStrArg(term, 2).toUpperCase();
            if (!product.equals("KWX") && !product.equals("KWE") &&
                !product.equals("KGS") && !product.equals("KSS") &&
                !product.equals("KG1") && !product.equals("KS1"))
               problem = "Invalid product code.";

            // Check for Std vs. Pro/Ent
            if (new File(runtimeDir + "java").exists() && !product.endsWith("X") && !product.endsWith("E"))
               problem = "Mismatched product code.";

            // Check Jig
            String supportJigPath = Utils.findClasspathFile("support.xpl", "jigs");
            if (supportJigPath.length() > 0 && !product.substring(1,2).equals("S") &&
                !product.substring(1,2).equals("W"))
               problem = "Mismatched product code.";
            String basicJigPath = Utils.findClasspathFile("basic.xpl", "jigs");
            if (basicJigPath.length() > 0 && !product.substring(1,2).equals("G") &&
                !product.substring(1,2).equals("W"))
               problem = "Mismatched product code.";
            if (supportJigPath.length() > 0 && basicJigPath.length() > 0 &&
                (product.equals("KWE") || product.equals("KWX")))
               problem = null;
            if (supportJigPath.length() == 0 && basicJigPath.length() == 0)
               problem = "Product installation corrupted; re-install.";

            // Set License Type
            if (product.endsWith("X")) licenseType = "professional";
            if (product.endsWith("S")) licenseType = "standard";
            if (product.endsWith("1")) licenseType = "personal";
            if (product.endsWith("E")) licenseType = "professional";

            // Check the version
            String version = ls.GetStrArg(term, 4);
            if (!version.startsWith("4"))
               problem = "Invalid version number.";

            // Check the operating system
            String os = System.getProperty("os.name").toUpperCase();
            String platform = ls.GetStrArg(term, 3);
            if ((os.indexOf("WINDOWS") >= 0 && !platform.equalsIgnoreCase("PC")) ||
               (os.indexOf("LINUX") >= 0 && !platform.equalsIgnoreCase("LX")))
               problem = "Invalid platform.";

            Calendar today = Calendar.getInstance();
            int year = ls.GetIntArg(term, 10);
            int month = ls.GetIntArg(term, 7);
            int day = ls.GetIntArg(term, 9);
            String monthName = ls.GetStrArg(term, 8);

            // If it's timelocked, check it first
            if (year != 0 || month != 0 || year != 0)
            {
              int relYear, relMonth, relDay;

              // KBV's expire on the date in the serial number
              if (product.equalsIgnoreCase("KWV"))
              {
                relYear = today.get(Calendar.YEAR);
                relMonth = today.get(Calendar.MONTH)+1;   // Java bug!
                relDay = today.get(Calendar.DAY_OF_MONTH);
              }
              //Everything else can only be used on releases before expiry
              else
              {
                relYear = Integer.parseInt(res.getString("ApplicationDate.Year"));
                relMonth = Integer.parseInt(res.getString("ApplicationDate.Month"));
                relDay = Integer.parseInt(res.getString("ApplicationDate.Day"));
              }

              if ((relYear > year) ||
                 (relYear == year && relMonth > month) ||
                 (relYear == year && relMonth == month && relDay > day))
                 problem = "Registration expired on " + Integer.toString(day) + "-" +
                    monthName + "-" + Integer.toString(year) + ". Contact Amzi! to renew.";
            }

            // If not a KG1 or KS1 and has no expiry, something is wrong
            if (!product.equalsIgnoreCase("KG1") && (year == 0 || month == 0 || day == 0))
               problem = "Invalid registration details. Please try again.";
            if (!product.equalsIgnoreCase("KS1") && (year == 0 || month == 0 || day == 0))
               problem = "Invalid registration details. Please try again.";
         }

         setCursor(new Cursor(Cursor.WAIT_CURSOR));

         // If so register it
         if (problem == null)
         {
            s = "register:unl$rid('" + Utils.doubleSlashes(xplPath) + "', $" + name +
               "$, $" + org + "$, $$, $" + serial + "$, $" + unlock + "$)";
            term = ls.ExecStr(s);
            if (term == 0) problem = "Invalid registration details. Please try again.";
         }

         setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
         ls.Close();
      }
      catch (LSException ex)
      {
         setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
         if (ls != null) try {ls.Close();} catch (LSException ex2) { }
         problem = "Registration failed (exception): " + ex.getMessage() + ".";
      }
      return problem;
   }

   private void saveDetails(String license)
   {
      if (nameField.getText() != null)
         settings.set("license_name", nameField.getText());
      if (orgField.getText() != null)
         settings.set("license_organization", orgField.getText());
      if (serialField.getText() != null)
         settings.set("serial_number", serialField.getText());
      if (unlockField.getText() != null)
         settings.set("unlock_code", unlockField.getText());
      settings.set("license_type", license);
      try
      {
         settings.save();
      }
      catch (IOException ex)
      {
         JOptionPane.showMessageDialog(app, "I/O error saving preferences: " + ex.getMessage(),
            "ERROR", JOptionPane.ERROR_MESSAGE);
      }
   }

//----------------------------------------------------------------------------//

   // WindowListener

   protected void processWindowEvent(WindowEvent e)
   {
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
         cancelButton_actionPerformed(null);
   }
}