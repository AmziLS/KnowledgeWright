package amzi.ide;

import java.awt.*;
import java.io.*;
import javax.swing.*;
import com.borland.dbswing.ColumnLayout;
import java.awt.event.*;
import java.util.Locale;
import java.util.Vector;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

public class PreferencesDialog extends JDialog {
   JPanel mainPanel = new JPanel();
   BorderLayout mainBorderLayout = new BorderLayout();
   JPanel settingsPanel = new JPanel();
   GridBagLayout settingsGridBagLayout = new GridBagLayout();
   JLabel webBrowserLabel = new JLabel();
   JTextField webBrowserPath = new JTextField();
   JButton webBrowseButton = new JButton();
   JPanel buttonsPanel = new JPanel();
   ColumnLayout buttonsColumnLayout = new ColumnLayout();
   JButton okButton = new JButton();
   JButton cancelButton = new JButton();
   JLabel runLogDetailLabel = new JLabel();
   Object logDetailLevels[] = {"low", "medium", "high"};
   JComboBox runLogDetailComboBox = new JComboBox(logDetailLevels);
   JLabel refreshStatusLabel = new JLabel();
   JCheckBox refreshStatus = new JCheckBox();
   JLabel unicodeLabel = new JLabel();
   JCheckBox unicodeSupport = new JCheckBox();
   JComboBox localeComboBox;
   JLabel localeLabel = new JLabel();
   Vector localeVector = new Vector();
   Locale localeArray[];

   App app;
   SavedSettings settings;

   public PreferencesDialog(App app, SavedSettings settings)
   {
      super(app, "Preferences", true);
      this.app = app;
      this.settings = settings;
      try
      {
         jbInit();

         // To add a new value, first put a default in SavedSettings,
         // then load the value here and save it in the OK button action
         webBrowserPath.setText(settings.get("web_browser"));
         runLogDetailComboBox.setSelectedItem(settings.get("run_log_detail"));
         if (settings.get("auto_update_status").equals("true"))
            refreshStatus.setSelected(true);
         else
            refreshStatus.setSelected(false);
         if (settings.get("unicode").equals("true"))
            unicodeSupport.setSelected(true);
         else
            unicodeSupport.setSelected(false);
         pack();
      }
      catch(Exception ex)
      {
         ex.printStackTrace();
      }
   }

   void jbInit() throws Exception
   {
      mainPanel.setLayout(mainBorderLayout);
      settingsPanel.setLayout(settingsGridBagLayout);
      webBrowserLabel.setText("Web Browser Path:");
      if (webBrowserPath.getColumns() < 40)
         webBrowserPath.setColumns(40);
      webBrowseButton.setActionCommand("webBrowseButton");
      webBrowseButton.setText("Browse");
      webBrowseButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            webBrowseButton_actionPerformed(e);
         }
      });
      buttonsColumnLayout.setHorizontalFill(true);
      buttonsPanel.setLayout(buttonsColumnLayout);

      this.getRootPane().setDefaultButton(okButton);
      okButton.setText("OK");
      okButton.setMnemonic(KeyEvent.VK_ENTER);
      okButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            okButton_actionPerformed(e);
         }
      });

      cancelButton.setText("Cancel");
      cancelButton.setMnemonic(KeyEvent.VK_ESCAPE);
      cancelButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            cancelButton_actionPerformed(e);
         }
      });
      runLogDetailLabel.setText("Run Log Detail:");
      runLogDetailComboBox.setEditable(false);
      refreshStatusLabel.setText("Auto-Update Status Window:");
      unicodeLabel.setText("Save kb files in Unicode Format:");

      localeLabel.setText("Locale:");
      localeArray = Locale.getAvailableLocales();
      for (int i = 0 ; i < localeArray.length ; i++)
         localeVector.addElement(localeArray[i].getDisplayName());
      localeComboBox = new JComboBox(localeVector);
      localeComboBox.setSelectedItem(Locale.getDefault().getDisplayName());

      getContentPane().add(mainPanel);
      mainPanel.add(settingsPanel, BorderLayout.CENTER);
      settingsPanel.add(webBrowserLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(webBrowserPath, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(webBrowseButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(runLogDetailLabel, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(runLogDetailComboBox, new GridBagConstraints(1, 2, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(refreshStatusLabel, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(refreshStatus, new GridBagConstraints(1, 4, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(unicodeLabel, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(unicodeSupport, new GridBagConstraints(1, 6, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(localeLabel, new GridBagConstraints(0, 8, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));
      settingsPanel.add(localeComboBox, new GridBagConstraints(1, 8, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(2, 2, 2, 2), 0, 0));

      // Must be last to get the focus on the first field
      mainPanel.add(buttonsPanel, BorderLayout.EAST);
      buttonsPanel.add(okButton, null);
      buttonsPanel.add(cancelButton, null);
   }

   void webBrowseButton_actionPerformed(ActionEvent e)
   {
      File f = app.selectSourceFile("Select Web Browser", "", JFileChooser.OPEN_DIALOG);
      if (f != null)
         webBrowserPath.setText(f.getAbsolutePath());
   }

   void okButton_actionPerformed(ActionEvent e)
   {
      settings.set("web_browser", webBrowserPath.getText());
      settings.set("run_log_detail", (String)runLogDetailComboBox.getSelectedItem());
      if (refreshStatus.isSelected())
         settings.set("auto_update_status", "true");
      else
         settings.set("auto_update_status", "false");
      if (unicodeSupport.isSelected())
         settings.set("unicode", "true");
      else
         settings.set("unicode", "false");
      settings.set("locale", localeArray[localeComboBox.getSelectedIndex()].toString());

      try
      {
         settings.save();
      }
      catch (IOException ex)
      {
         JOptionPane.showMessageDialog(app, "I/O error saving preferences: " + ex.getMessage(),
            "ERROR", JOptionPane.ERROR_MESSAGE);
      }
      this.dispose();
   }

   void cancelButton_actionPerformed(ActionEvent e)
   {
      this.dispose();
   }
}