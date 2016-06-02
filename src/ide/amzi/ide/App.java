// This snippet creates a new class extending JFrame whose content pane is a
// JDesktopPane.  Use it together with the InternalFrame snippet to create
// the structure of an MDI (multiple document interface) application.

// Instructions:
// 1. Create a DesktopPane snippet. Be sure to specify the "Name of Internal
//    Frame" parameter.  The snippet has a main method, so is usually placed
//    in a new, empty project.
// 2. Create an InternalFrame snippet, specifying the same class name.
// 3. You can now compile and run the project. Use the File menu to open new
//    internal frames.

// On March 21, 2002 Dennis Merritt figured out the golf swing. "It's all
// uphill from here." he thinks.

// some hints for threads (relevant for Swing):
// - updating a Component should ONLY happen in the event-dispatching thread (AWT)
// - performing time consuming calculations should NOT happen in the AWT-Thread
// actionPerformed is the AWT thread

//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package amzi.ide;

import amzi.ls.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.UnsatisfiedLinkError;
import java.net.*;
import java.util.*;
import javax.swing.*;
import com.borland.dbswing.FontChooser;

public class App extends JFrame
{
   public final static boolean HTML_ENGINE = true;
   public final static String CONFIG_FILE = "kw.cfg";
   public final static int REOPEN_LIST_SIZE = 10;

   public String initialDir, docsDirectory, tempDirectory, runtimeDirectory;
   public static ResourceBundle resources;

   BorderLayout borderLayout = new BorderLayout();
   ResourceBundle res = ResourceBundle.getBundle("amzi.ide.resources.IDE", Locale.getDefault());
   private JDesktopPane desktopPane = new JDesktopPane();
   JMenuBar menuBar = new JMenuBar();
   JMenu menuKnowledgebase = new JMenu();
   JMenuItem menuKBNew = new JMenuItem();
   JMenuItem menuKBOpen = new JMenuItem();
   JMenu menuKBReopen = new JMenu();
   JMenuItem menuKBSave = new JMenuItem();
   JMenuItem menuKBSaveAs = new JMenuItem();
   JMenuItem menuKBClose = new JMenuItem();
//   JMenuItem menuKBPrint = new JMenuItem();
   JMenuItem menuKBUnlock = new JMenuItem();
   JMenuItem menuKBExit = new JMenuItem();
   JMenu menuSearch = new JMenu();
   JMenuItem menuSearchFind = new JMenuItem();
   JMenu menuView = new JMenu();
   JMenuItem menuViewConnections = new JMenuItem();
   JMenuItem menuViewProblems = new JMenuItem();
   JMenu menuRun = new JMenu();
   JMenuItem menuRunRun = new JMenuItem();
   JMenuItem menuRunSingleStep = new JMenuItem();
   JMenu menuRunLogDetail = new JMenu();
   ButtonGroup logGroup = new ButtonGroup();
   JRadioButtonMenuItem menuLogNone = new JRadioButtonMenuItem();
   JRadioButtonMenuItem menuLogLow = new JRadioButtonMenuItem();
   JRadioButtonMenuItem menuLogMedium = new JRadioButtonMenuItem();
   JRadioButtonMenuItem menuLogHigh = new JRadioButtonMenuItem();
   JMenu menuTools = new JMenu();
   JMenuItem menuRunCompile = new JMenuItem();
   JMenuItem menuToolsConvert = new JMenuItem();
   JMenuItem menuToolsPreferences = new JMenuItem();
   JMenuItem menuToolsFont = new JMenuItem();
   JMenu menuHelp = new JMenu();
   JMenuItem menuHelpContents = new JMenuItem();
   JMenuItem menuHelpTutorial = new JMenuItem();
   JMenuItem menuHelpLicense = new JMenuItem();
   JMenuItem menuHelpAmzi = new JMenuItem();
   JMenuItem menuHelpSupport = new JMenuItem();
   JMenuItem menuHelpForums = new JMenuItem();
   JMenuItem menuHelpAbout = new JMenuItem();
   JFileChooser fileChooser = new JFileChooser();

   private Vector openFileList = new Vector (10,2);
   private int lastEditXPos = 0;
   private int lastEditYPos = 0;
   private String editXPL = "kwauthor.xpl";
   private SavedSettings settings;
   private StatusTabFrame statusFrame = null;
   private Font defaultFont = null;
   private String lastOpenPath = "../samples";
   private Cursor lastCursor;
   private boolean unregistered = true;
//   private static Class classEditFrame;
   private static Class classKBTreeFrame;

   static
   {
      // Find our resources
      try
      {
         resources = ResourceBundle.getBundle("amzi.ide.resources.IDE", Locale.getDefault());
      }
      catch (MissingResourceException mre)
      {
         JOptionPane.showMessageDialog(null, "File amzi.ide.resources/IDE.properties not found",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         System.exit(1);
      }

      // Load our libraries
      String startDir;
      String workshop = Utils.findClasspathFile("kwauthor.xpl", "");
      if (!workshop.equals(""))
         startDir = workshop.substring(0, workshop.length()-12);
      else
         startDir = System.getProperty("user.dir") + System.getProperty("file.separator");

      // Try loading the libraries
      if (System.getProperty("os.name").indexOf("indow") >= 0)
      {
         try
         {
            Runtime.getRuntime().load(startDir + "amzi.dll");
            Runtime.getRuntime().load(startDir + "amzijni.dll");
            Runtime.getRuntime().load(startDir + "aosutils.lsx");
            Runtime.getRuntime().load(startDir + "aodbc.lsx");
//            JOptionPane.showMessageDialog(this, "Loaded dlls and lsxs", "", JOptionPane.PLAIN_MESSAGE);
         }
         catch (UnsatisfiedLinkError ex)
         {
            try
            {
               Runtime.getRuntime().loadLibrary("amzi");
               Runtime.getRuntime().loadLibrary("amzijni");
//               JOptionPane.showMessageDialog(this, "Load Libraried dlls", "", JOptionPane.PLAIN_MESSAGE);
            }
            catch (UnsatisfiedLinkError ex2)
            {
//               JOptionPane.showMessageDialog(null, "Unable to find libraries and lsx's (amzi, amzijni, aosutils): " + ex2.getMessage() + "\nRe-install or Repair KnowledgeWright", "Error", JOptionPane.ERROR_MESSAGE);
            }
         }
      }
      else
      {
         try
         {
            Runtime.getRuntime().load(startDir + "libamzi.so");
            Runtime.getRuntime().load(startDir + "libamzijni.so");
            Runtime.getRuntime().load(startDir + "aosutils.lsx");
         }
         catch (UnsatisfiedLinkError ex)
         {
            try
            {
               Runtime.getRuntime().loadLibrary("amzi");
               Runtime.getRuntime().loadLibrary("amzijni");
            }
            catch (UnsatisfiedLinkError ex2)
            {
//               JOptionPane.showMessageDialog(null, "Unable to find libraries and lsx's (amzi, amzijni, aosutils): " + ex2.getMessage() + "\nRe-install or Repair KnowledgeWright", "Error", JOptionPane.ERROR_MESSAGE);
            }
         }
      }
   }

   public App(String[] args)
   {
      boolean personal = true;
      String product = null;

      // Check the Java version
      if (!System.getProperty("java.version").startsWith("1.3"))
         JOptionPane.showMessageDialog(this, "This program is designed to run with Java 1.3" +
            "--you are running version " + System.getProperty("java.version"),
            "WARNING", JOptionPane.WARNING_MESSAGE);

//      enableEvents(AWTEvent.WINDOW_EVENT_MASK);

//      JOptionPane.showMessageDialog(this, System.getProperty("java.class.path"), "", JOptionPane.PLAIN_MESSAGE);

      // If we were started from a strange place set the initial directory to workshop so
      // the xpl files can be found
      String workshop = Utils.findClasspathFile("kwauthor.xpl", "");

//      JOptionPane.showMessageDialog(this, workshop, "", JOptionPane.PLAIN_MESSAGE);

      if (!workshop.equals(""))
         initialDir = workshop.substring(0, workshop.length()-12);
      else
         initialDir = System.getProperty("user.dir") + System.getProperty("file.separator");

//      JOptionPane.showMessageDialog(this, initialDir, "", JOptionPane.PLAIN_MESSAGE);

      // Save other environment info
      docsDirectory = new File(initialDir).getParent() + System.getProperty("file.separator") +
         "docs" + System.getProperty("file.separator");
      tempDirectory = new File(initialDir).getParent() + System.getProperty("file.separator") +
         "temp" + System.getProperty("file.separator");
      runtimeDirectory = new File(initialDir).getParent() + System.getProperty("file.separator") +
         "runtime" + System.getProperty("file.separator");

//      JOptionPane.showMessageDialog(this, docsDirectory, "", JOptionPane.PLAIN_MESSAGE);

      try
      {
         // Get the preferences (do this before jbInit for screen size)
         settings = new SavedSettings();

         // Set up class types
//         classEditFrame = Class.forName("amzi.ide.EditFrame");
         classKBTreeFrame = Class.forName("amzi.ide.KBTreeFrame");
         jbInit();

         // Make us appear
         this.setVisible(true);
         lastCursor = getCursor();

         // Set Locale
         if (getConfigEntry("locale").length() > 0)
         {
            String[] parts = parseLocale(getConfigEntry("locale"));
            Locale.setDefault(new Locale(parts[0], parts[1], parts[2]));
         }

         // Set the default for the run log detail after jbInit
         String logDetail = getConfigEntry("run_log_detail");
         if (logDetail.equals("low")) menuLogLow.setSelected(true);
         if (logDetail.equals("medium")) menuLogMedium.setSelected(true);
         if (logDetail.equals("high")) menuLogHigh.setSelected(true);
         if (logDetail.equals("none")) menuLogNone.setSelected(true);

         // Set the default font
         if (getConfigEntry("font_name").length() > 0 && !getConfigEntry("font_size").equals("0"))
            defaultFont = new Font(getConfigEntry("font_name"), new Integer(getConfigEntry("font_style")).intValue(),
               new Integer(getConfigEntry("font_size")).intValue());
         else
            defaultFont = new Font("Dialog", Font.PLAIN, 12);

         // Put up the license, if needed
         String userName = getConfigEntry("user_name");
         String osName = getConfigEntry("os_name");
         String osVersion = getConfigEntry("os_version");
         if (userName.length() == 0 || osName.length() == 0 || osVersion.length() == 0 ||
            !userName.equals(System.getProperty("user.name")) || !osName.equals(System.getProperty("os.name")) ||
            !osVersion.equals(System.getProperty("os.version")) )
         {
            LicenseDialog license = new LicenseDialog(this);
            license.show();
            if (license.getAccepted() == false)
               System.exit(0);
            else
            {
               setConfigEntry("user_name", System.getProperty("user.name"));
               setConfigEntry("os_name", System.getProperty("os.name"));
               setConfigEntry("os_version", System.getProperty("os.version"));
            }
         }

         // Check if we have been registered
         unregistered = true;
         LogicServer ls = null;
         try
         {
            ls = new LogicServer();
            ls.Init("");
            String xplPath = Utils.findClasspathFile(editXPL, "");
            ls.Load(xplPath);
            long term = ls.ExecStr("register:unl$gri(_User, _Org, _Info, _Prod, _Key)");
            if (term == 0) unregistered = true;
            else
            {
               unregistered = false;

               // Get the product
               String serial = ls.GetStrArg(term, 4);
               product = serial.substring(0, 3).toUpperCase();

               if (product.equals("KG1") || product.equals("KS1")) personal = true;
               else personal = false;

               // This will fail (return term = 0) if it is expired
               term = ls.ExecStr("register:unl$ctm(_XMonth, _XMonthName, _XDay, _XYear)");

               // If Professional, just a reminder
               if (term == 0 && (product.equals("KWX") || product.equals("KWE")))
               {
                  long lastReminder = Long.valueOf(getConfigEntry("maintenance_reminder")).longValue();
                  long threeDaysAgo = (System.currentTimeMillis() - 3*24*3600*1000);
                  if (term == 0 && lastReminder != 0 &&  lastReminder < threeDaysAgo)
                  {
                     Object buttons[] = {"Remind Me Later", "Don't Remind Me Again"};
                     JOptionPane pane = new JOptionPane("Product maintenance has expired for this license. Please contact Amzi! to renew.",
                        JOptionPane.WARNING_MESSAGE, 0, null, buttons, "Remind Me Later");
                     JDialog dialog = pane.createDialog(this, "Maintenance Expired");
                     dialog.show();
                     String choice = (String)pane.getValue();
                     if (choice.equals("Remind Me Later"))
                        setConfigEntry("maintenance_reminder", Long.toString(System.currentTimeMillis()));
                     else
                        setConfigEntry("maintenance_reminder", "0");
                     saveConfig();
                  }
               }

               // If Evaluation and expired, we go bye-bye
/*               if (term == 0 && product.equals("KBV"))
               {
                  JOptionPane.showMessageDialog(this, "This Evaluation Copy has expired. Please contact Amzi! to renew.",
                     "Product Expired", JOptionPane.ERROR_MESSAGE);
                  ls.Close();
                  System.exit(0);
               } */
            }

            ls.Close();
         }
         catch (LSException ex)
         {
           JOptionPane.showMessageDialog(this, ex.getMessage(),
               "ERROR", JOptionPane.ERROR_MESSAGE);

            if (ls != null) ls.Close();
            unregistered = true;
            if (product == null || product.equals("KBV"))
               System.exit(0);
         }

         // Put up the splash screen for academic/personal/evaluation
         if (unregistered || personal)
         {
            PersonalLicense pl = new PersonalLicense(this, desktopPane.getSize());
            pl.show();
            try { Thread.sleep(5000); } catch (InterruptedException ex) { }
            pl.dispose();
         }
         else
            menuKBUnlock.setEnabled(false);

         // Now put up the menu bar
         this.setJMenuBar(menuBar);

         // If a kb file was passed as an argument, open it
         // Otherwise, reopen the last opened kb if it was left open
         if (args.length > 0 || getConfigEntry("kb_open").equals("true"))
         {
            // Open it
            try
            {
               String kbname = "";

               // Stupid Windows passes paths with spaces unquoted, so these
               // actually appear as multiple arguments
               if (args.length > 0)
               {
                  for (int i = 0 ; i < args.length ; i ++)
                     kbname = kbname + args[i] + " ";
                  kbname = kbname.trim();
               }
               else kbname = menuKBReopen.getItem(0).getText();

               File f = new File(kbname);
               if (f != null)
               {
                  KB kb = new KB(this, f, editXPL);
                  openKB(kb);
               }
            }
            catch (KBUserException ex)
            {
               JOptionPane.showMessageDialog(this, ex.getMessage(),
                  ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            }
         }
         else
            menuKBOpen_actionPerformed(null);
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            "INTERNAL ERROR", JOptionPane.ERROR_MESSAGE);
         ex.printStackTrace();
      }
   }

   public void jbInit() throws Exception
   {
      this.getContentPane().setLayout(borderLayout);
//      this.getContentPane().setBackground(SystemColor.windowBorder);
      this.setContentPane(desktopPane);
      this.setForeground(SystemColor.windowText);

      // Put us where we were last time, unless there was no last time
      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      this.setLocation(new Integer(getConfigEntry("screen_x")).intValue(), new Integer(getConfigEntry("screen_y")).intValue());
      if (getConfigEntry("screen_w").equals("0") || getConfigEntry("screen_h").equals("0"))
         this.setSize(screenSize.width * 3 / 4, screenSize.height * 3 / 4);
      else
         this.setSize(new Integer(getConfigEntry("screen_w")).intValue(), new Integer(getConfigEntry("screen_h")).intValue());

      this.setTitle(resources.getString("ApplicationTitle.Text"));
      ImageIcon icon = new ImageIcon(getClass().getResource("resources/ide_icon.gif"));
      this.setIconImage(icon.getImage());
      desktopPane.setBackground(SystemColor.controlShadow);
      desktopPane.setForeground(SystemColor.textText);
      desktopPane.putClientProperty("JDesktopPane.dragMode", "outline");

      // Knowledgebase Menu
      menuKnowledgebase.setText(res.getString("menuKnowledgebase.Text"));
      menuKnowledgebase.setMnemonic(KeyEvent.VK_K);

      menuKBNew.setText(res.getString("menuNew.Text"));
      menuKBNew.setMnemonic(KeyEvent.VK_N);
      menuKBNew.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBNew_actionPerformed(e);
         }
      });
	   menuKBNew.setToolTipText("Create a new knowledgebase");
      menuKBNew.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBNew.setIcon(new ImageIcon(getClass().getResource("resources/new.gif")));

      menuKBOpen.setText(res.getString("menuOpen.Text"));
      menuKBOpen.setMnemonic(KeyEvent.VK_O);
      menuKBOpen.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBOpen_actionPerformed(e);
         }
      });
	   menuKBOpen.setToolTipText("Open an existing knowledgebase for editting");
      menuKBOpen.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBOpen.setIcon(new ImageIcon(getClass().getResource("resources/open.gif")));

      menuKBReopen.setText("Re-open");
      menuKBReopen.setMnemonic(KeyEvent.VK_R);
      menuKBReopen.setToolTipText("Re-open a recent knowledgebase for editting");
      menuKBReopen.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBReopen.setIcon(new ImageIcon(getClass().getResource("resources/open.gif")));
      for (int i = 0 ; i < App.REOPEN_LIST_SIZE ; i++)
      {
         String istr = new Integer(i).toString();
         if (getConfigEntry("open"+istr).length() > 0)
         {
            JMenuItem reopenFile = new JMenuItem(getConfigEntry("open"+istr));
            reopenFile.addActionListener(new ActionListener()
            {
               public void actionPerformed(ActionEvent e)
               {
                  menuKBReopen_actionPerformed(e);
               }
            });
            menuKBReopen.add(reopenFile);
         }
      }

      menuKBSave.setText(res.getString("menuSave.Text"));
      menuKBSave.setMnemonic(KeyEvent.VK_S);
      menuKBSave.setEnabled(false);
      menuKBSave.setToolTipText("Save the open knowledgebase");
	   menuKBSave.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBSave.setIcon(new ImageIcon(getClass().getResource("resources/save.gif")));
      menuKBSave.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBSave_actionPerformed(e);
         }
      });

      menuKBSaveAs.setText("SaveAs");
      menuKBSaveAs.setMnemonic(KeyEvent.VK_A);
      menuKBSaveAs.setEnabled(false);
      menuKBSaveAs.setToolTipText("Save the open knowledgebase to a new file");
	   menuKBSaveAs.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBSaveAs.setIcon(new ImageIcon(getClass().getResource("resources/saveas.gif")));
      menuKBSaveAs.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBSaveAs_actionPerformed(e);
         }
      });

      menuKBClose.setText(res.getString("menuClose.Text"));
      menuKBClose.setMnemonic(KeyEvent.VK_C);
      menuKBClose.setEnabled(false);
      menuKBClose.setToolTipText("Close the current knowledgebase");
	   menuKBClose.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBClose.setIcon(new ImageIcon(getClass().getResource("resources/close.gif")));
      menuKBClose.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBClose_actionPerformed(e);
         }
      });

      menuKBUnlock.setText("Unlock");
      menuKBUnlock.setMnemonic(KeyEvent.VK_L);
	   menuKBUnlock.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBUnlock.setIcon(new ImageIcon(getClass().getResource("resources/ide_icon.gif")));
      menuKBUnlock.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBUnlock_actionPerformed(e);
         }
      });

      menuKBExit.setText(res.getString("menuExit.Text"));
      menuKBExit.setMnemonic(KeyEvent.VK_X);
	   menuKBExit.setHorizontalTextPosition(JButton.RIGHT);
	   menuKBExit.setIcon(new ImageIcon(getClass().getResource("resources/blank.gif")));
      menuKBExit.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBExit_actionPerformed(e);
         }
      });

      // Search Menu
      menuSearch.setText("Search");
      menuSearch.setMnemonic(KeyEvent.VK_S);

      menuSearchFind.setText("Find");
      menuSearchFind.setMnemonic(KeyEvent.VK_F);
      menuSearchFind.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
      menuSearchFind.setEnabled(false);
	   menuSearchFind.setIcon(new ImageIcon(getClass().getResource("resources/find.gif")));
      menuSearchFind.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuSearchFind_actionPerformed(e);
         }
      });

      // View Menu
      menuView.setText("View");
      menuViewConnections.setText("Connections");
      menuViewConnections.setEnabled(false);
	   menuViewConnections.setIcon(new ImageIcon(getClass().getResource("resources/web.gif")));
      menuViewConnections.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuViewConnections_actionPerformed(e);
         }
      });
      menuViewProblems.setText("Problems");
      menuViewProblems.setEnabled(false);
	   menuViewProblems.setIcon(new ImageIcon(getClass().getResource("resources/magnifier.gif")));
      menuViewProblems.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuViewProblems_actionPerformed(e);
         }
      });

      // Run Menu
      menuRun.setText("Run");
      menuRun.setMnemonic(KeyEvent.VK_R);

      menuRunRun.setText("Run");
      menuRunRun.setMnemonic(KeyEvent.VK_R);
      menuRunRun.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0));
      menuRunRun.setEnabled(false);
	   menuRunRun.setToolTipText("Run a knowledgebase");
      menuRunRun.setHorizontalTextPosition(JButton.RIGHT);
	   menuRunRun.setIcon(new ImageIcon(getClass().getResource("resources/run.gif")));
      menuRunRun.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuRunRun_actionPerformed(e, false);
         }
      });

      menuRunSingleStep.setText("Single Step");
      menuRunSingleStep.setMnemonic(KeyEvent.VK_S);
      menuRunSingleStep.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, ActionEvent.SHIFT_MASK));
      menuRunSingleStep.setEnabled(false);
      menuRunSingleStep.setToolTipText("Debug a knowledgebase one step at a time");
      menuRunSingleStep.setIcon(new ImageIcon(getClass().getResource("resources/step.gif")));
      menuRunSingleStep.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuRunRun_actionPerformed(e, true);
         }
      });

      menuRunLogDetail.setText("Log Detail");
      menuRunLogDetail.setMnemonic(KeyEvent.VK_L);
      menuRunLogDetail.setIcon(new ImageIcon(getClass().getResource("resources/hand_pen.gif")));
      menuLogLow.setText("Low");
      menuLogLow.setMnemonic(KeyEvent.VK_L);
      logGroup.add(menuLogLow);
      menuLogMedium.setText("Medium");
      menuLogMedium.setMnemonic(KeyEvent.VK_M);
      logGroup.add(menuLogMedium);
      menuLogHigh.setText("High");
      menuLogHigh.setMnemonic(KeyEvent.VK_H);
      logGroup.add(menuLogHigh);
      menuLogNone.setText("None");
      menuLogNone.setMnemonic(KeyEvent.VK_N);
//      logGroup.add(menuLogNone);

      menuRunCompile.setText("Compile knowledgebase");
      menuRunCompile.setMnemonic(KeyEvent.VK_C);
      menuRunCompile.setEnabled(false);
      menuRunCompile.setToolTipText("Compile a knowledgebase for deployment");
      menuRunCompile.setHorizontalTextPosition(JButton.RIGHT);
	   menuRunCompile.setIcon(new ImageIcon(getClass().getResource("resources/arrow_4diamonds.gif")));
      menuRunCompile.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuRunCompile_actionPerformed(e);
         }
      });

      // Tools Menu
      menuTools.setText("Tools");
      menuTools.setMnemonic(KeyEvent.VK_T);

      menuToolsConvert.setText("Convert WebLS to Basic");
      menuToolsConvert.setMnemonic(KeyEvent.VK_C);
      menuToolsConvert.setToolTipText("Convert a WebLS logicbase to a KnowledgeWright Basic knowledgebase");
      menuToolsConvert.setHorizontalTextPosition(JButton.RIGHT);
	   menuToolsConvert.setIcon(new ImageIcon(getClass().getResource("resources/convert.gif")));
      menuToolsConvert.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuToolsConvert_actionPerformed(e);
         }
      });

      menuToolsFont.setText("Font");
      menuToolsFont.setMnemonic(KeyEvent.VK_F);
      menuToolsFont.setToolTipText("Set the font");
      menuToolsFont.setHorizontalTextPosition(JButton.RIGHT);
	   menuToolsFont.setIcon(new ImageIcon(getClass().getResource("resources/font.gif")));
      menuToolsFont.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuToolsFont_actionPerformed(e);
         }
      });

      menuToolsPreferences.setText("Preferences");
      menuToolsPreferences.setMnemonic(KeyEvent.VK_P);
      menuToolsPreferences.setToolTipText("Set configuration options for this program");
      menuToolsPreferences.setHorizontalTextPosition(JButton.RIGHT);
	   menuToolsPreferences.setIcon(new ImageIcon(getClass().getResource("resources/computer.gif")));
      menuToolsPreferences.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            menuToolsPreferences_actionPerformed(e);
         }
      });

      // Help Menu
      menuHelp.setText(res.getString("menuHelp.Text"));
      menuHelp.setMnemonic(KeyEvent.VK_H);

      menuHelpAbout.setText(res.getString("menuAbout.Text"));
      menuHelpAbout.setMnemonic(KeyEvent.VK_A);
	   menuHelpAbout.setIcon(new ImageIcon(getClass().getResource("resources/up_arrow.gif")));
      menuHelpAbout.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpAbout_actionPerformed(e);
         }
      });

      menuHelpTutorial.setText("Tutorial");
      menuHelpTutorial.setMnemonic(KeyEvent.VK_W);
      menuHelpTutorial.setIcon(new ImageIcon(getClass().getResource("resources/hand_pen.gif")));
      menuHelpTutorial.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpTutorial_actionPerformed(e);
         }
      });

      menuHelpSupport.setText("Technical Support Website");
      menuHelpSupport.setMnemonic(KeyEvent.VK_W);
      menuHelpSupport.setIcon(new ImageIcon(getClass().getResource("resources/question.gif")));
      menuHelpSupport.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpSupport_actionPerformed(e);
         }
      });

      menuHelpForums.setText("Web Support Forums");
      menuHelpForums.setMnemonic(KeyEvent.VK_W);
      menuHelpForums.setIcon(new ImageIcon(getClass().getResource("resources/computer.gif")));
      menuHelpForums.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpForums_actionPerformed(e);
         }
      });

      menuHelpAmzi.setText("Amzi! Website");
      menuHelpAmzi.setMnemonic(KeyEvent.VK_W);
	   menuHelpAmzi.setIcon(new ImageIcon(getClass().getResource("resources/ide_icon.gif")));
      menuHelpAmzi.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpAmzi_actionPerformed(e);
         }
      });

      menuHelpContents.setText(res.getString("menuContents.Text"));
      menuHelpContents.setMnemonic(KeyEvent.VK_C);
      menuHelpContents.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
	   menuHelpContents.setIcon(new ImageIcon(getClass().getResource("resources/book.gif")));
      menuHelpContents.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpContents_actionPerformed(e);
         }
      });

      menuHelpLicense.setText("License");
      menuHelpLicense.setMnemonic(KeyEvent.VK_L);
	   menuHelpLicense.setIcon(new ImageIcon(getClass().getResource("resources/textpage.gif")));
      menuHelpLicense.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuHelpLicense_actionPerformed(e);
         }
      });

      // Menu bar
      menuBar.add(menuKnowledgebase);
      menuBar.add(menuSearch);
//      menuBar.add(menuView);
      menuBar.add(menuRun);
      menuBar.add(menuTools);
      menuBar.add(menuHelp);
      menuKnowledgebase.add(menuKBNew);
      menuKnowledgebase.add(menuKBOpen);
      menuKnowledgebase.add(menuKBReopen);
      menuKnowledgebase.addSeparator();
      menuKnowledgebase.add(menuKBSave);
      menuKnowledgebase.add(menuKBSaveAs);
      menuKnowledgebase.addSeparator();
      menuKnowledgebase.add(menuKBClose);
      menuKnowledgebase.addSeparator();
      menuKnowledgebase.add(menuKBUnlock);
      menuKnowledgebase.addSeparator();
//      menuKnowledgebase.add(menuKBPrint);
//      menuKnowledgebase.addSeparator();
      menuKnowledgebase.add(menuKBExit);
      menuSearch.add(menuSearchFind);
      menuHelp.add(menuHelpContents);
      menuHelp.add(menuHelpTutorial);
      menuHelp.add(menuHelpLicense);
      menuHelp.addSeparator();
      menuHelp.add(menuHelpAmzi);
      menuHelp.add(menuHelpSupport);
      menuHelp.add(menuHelpForums);
      menuHelp.addSeparator();
      menuHelp.add(menuHelpAbout);
      menuView.add(menuViewConnections);
      menuView.add(menuViewProblems);
      menuRun.add(menuRunRun);
      menuRun.add(menuRunSingleStep);
      menuRun.addSeparator();
      menuRun.add(menuRunLogDetail);
      menuRun.addSeparator();
      menuRun.add(menuRunCompile);
      menuTools.add(menuToolsPreferences);
      menuTools.add(menuToolsFont);
      if (this.HTML_ENGINE)
      {
         menuTools.addSeparator();
         menuTools.add(menuToolsConvert);
      }
      menuRunLogDetail.add(menuLogLow);
      menuRunLogDetail.add(menuLogMedium);
      menuRunLogDetail.add(menuLogHigh);
//      menuRunLogDetail.add(menuLogNone);
  }

//----------------------------------------------------------------------------//

   // Main Program
   static public void main(String[] args)
   {
      try
      {
         UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      }
      catch(Exception ex)
      {
         JOptionPane.showMessageDialog(null, ex.getMessage(),
            "Internal Error", JOptionPane.ERROR_MESSAGE);
//         System.out.println("INTERNAL ERROR: Cannot getSystemLookAndFeelClassName");
      }

      App desktop = new App(args);
      desktop.setVisible(true);
  }

//----------------------------------------------------------------------------//

   // KB | New
   private void menuKBNew_actionPerformed(ActionEvent e)
   {
      String jig;

      // Get the new KB name
      File f = selectSourceFile(res.getString("menuNew.Text"), "kb", JFileChooser.SAVE_DIALOG);
      if (f == null) return;
      if (f.exists())
         if (JOptionPane.showConfirmDialog(this, "OK to overwrite existing file?",
            res.getString("menuNew.Text"), JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
            return;

      // Get the jig name
      String jigFiles[] = jigList();
      Object jigObj = JOptionPane.showInputDialog(this, "Select knowledgebase type", "New Knowledgebase Type",
         JOptionPane.QUESTION_MESSAGE, null, jigFiles, jigFiles[0]);
      if (jigObj == null) return;
      jig = jigObj.toString();

      // Create the new KB
      try
      {
         KB kb = new KB(this, f, editXPL, jig);
         openKB(kb);
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // KB | Open
   private void menuKBOpen_actionPerformed(ActionEvent e)
   {
      long start = System.currentTimeMillis();

      // Get the KB name
      if (findOpenKB() != null)
         if (!okToClose(JOptionPane.YES_NO_CANCEL_OPTION))
            return;
      File f = selectSourceFile("Open", "kb", JFileChooser.OPEN_DIALOG);
      if (f == null) return;

      // Open it
      try
      {
         KB kb = new KB(this, f, editXPL);
         openKB(kb);
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // KB | Reopen
   private void menuKBReopen_actionPerformed(ActionEvent e)
   {
      File f = new File(e.getActionCommand());

      // Open it
      try
      {
         KB kb = new KB(this, f, editXPL);
         openKB(kb);
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   private void openKB(KB kb)
   {
      boolean check;

      // Enable/disable menu items accordingly
      toggleKBMenu(true);

      // Let the user know we're doing something
      startWait();

      // Get the app size for positioning everything
      Dimension deskSize = desktopPane.getSize();

      // Put up the status tab pane
      statusFrame = new StatusTabFrame(this, kb);
      if (getConfigEntry("status_w").equals("0") || getConfigEntry("status_h").equals("0"))
         statusFrame.setBounds(0, deskSize.height*4/5, deskSize.width, deskSize.height*1/5);
      else
      {
         int x = new Integer(getConfigEntry("status_x")).intValue();
         int y = new Integer(getConfigEntry("status_y")).intValue();
         int w = new Integer(getConfigEntry("status_w")).intValue();
         int h = new Integer(getConfigEntry("status_h")).intValue();
         statusFrame.setBounds(x, y, w, h);
      }
      desktopPane.add(statusFrame);
//      desktopPane.getDesktopManager().activateFrame(statusFrame);

      // Do a status check if needed
      if (getConfigEntry("auto_update_status").equals("true"))
         statusFrame.updateDisplay(null, null);

      // Then the tree frame
      KBTreeFrame frame = new KBTreeFrame(this, statusFrame, kb, settings, docsDirectory);
      if (getConfigEntry("tree_w").equals("0") || getConfigEntry("tree_h").equals("0"))
         frame.setBounds(0, 0, deskSize.width*1/4, deskSize.height*4/5);
      else
      {
         int x = new Integer(getConfigEntry("tree_x")).intValue();
//         if (x < desktopPane.getLocationOnScreen().x) x = desktopPane.getLocationOnScreen().x;
         int y = new Integer(getConfigEntry("tree_y")).intValue();
//         if (y < desktopPane.getLocationOnScreen().y) y = desktopPane.getLocationOnScreen().y;
         int w = new Integer(getConfigEntry("tree_w")).intValue();
//         if (w > desktopPane.getSize().width) w = desktopPane.getSize().width;
         int h = new Integer(getConfigEntry("tree_h")).intValue();
//         if (h > desktopPane.getSize().height) h = desktopPane.getSize().height;
         frame.setBounds(x, y, w, h);
      }

      desktopPane.add(frame);
//      desktopPane.getDesktopManager().activateFrame(frame);

      // Add to the reopen list
      addReopenFile(kb.getPath());

      // OK to do stuff now
      stopWait();

      // Check if it's modified (via conversion)
      if (kb.isModified()) setKBModified(kb, true);

      // Check registration and record accordingly
      if (!kb.isRegistered())
         if (!getConfigEntry("license_type").equals("personal"))
         {
            setConfigEntry("license_type", "personal");
            saveConfig();
      }
   }

   private void addReopenFile(String path)
   {
      // First see if the file is already there
      for (int i = 0 ; i < menuKBReopen.getItemCount() ; i++)
         // If it is, move it to the top
         if (menuKBReopen.getItem(i).getText().equals(path))
         {
            JMenuItem reopenFile = menuKBReopen.getItem(i);
            menuKBReopen.remove(reopenFile);
            menuKBReopen.add(reopenFile, 0);
            return;
         }

      // If not, add it
      JMenuItem reopenFile = new JMenuItem(path);
      reopenFile.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            menuKBReopen_actionPerformed(e);
         }
      });
      menuKBReopen.add(reopenFile, 0);

      // And lop off the last one if we're full
      if (menuKBReopen.getItemCount() > App.REOPEN_LIST_SIZE) menuKBReopen.remove(App.REOPEN_LIST_SIZE);
   }

   // KB | Save
   private void menuKBSave_actionPerformed(ActionEvent e)
   {
      try
      {
         findOpenKB().getKB().save();
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // KB | SaveAs
   private void menuKBSaveAs_actionPerformed(ActionEvent e)
   {
      KB kb;
      String oldPath;

      File f = selectSourceFile(res.getString("menuSaveAs.Text"), "kb", JFileChooser.SAVE_DIALOG);
      if (f == null) return;
      try
      {
         // Find the open knowledgebase (only one allowed at a time)
         kb = findOpenKB().getKB();
         oldPath = kb.getPath();

         // Save it as a new file
         kb.saveAs(f.getPath());
         renameOpenFile(oldPath, kb.getPath());

         // Set the tree frame title
         JInternalFrame treeFrame = findOpenKB().getFrame();
         treeFrame.setTitle(kb.getName());
         treeFrame.repaint();
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
   }

   // KB | Close
   private void menuKBClose_actionPerformed(ActionEvent e)
   {
      OpenFile of;

      of = findOpenKB();
      if (of != null)
      {
         closeKB(of);
      }
   }

   // KB | License
   private void menuKBUnlock_actionPerformed(ActionEvent e)
   {
      // Create the dialog box
      RegisterDialog reg = new RegisterDialog(this, "Enter License Details", editXPL, runtimeDirectory, settings);

      // Center it
      Dimension deskSize = desktopPane.getSize();
      Dimension regSize = reg.getSize();
      reg.setLocation((deskSize.width-regSize.width)/2, (deskSize.height-regSize.height)/2);

      // And put it up
      reg.show();
   }

   // KB | Exit
   private void menuKBExit_actionPerformed(ActionEvent e)
   {
      if (okToClose(JOptionPane.YES_NO_CANCEL_OPTION))
         System.exit(0);
   }

   // Search | Find Text
   void menuSearchFind_actionPerformed(ActionEvent e)
   {
      OpenFile of = findOpenKB();
      if (of == null) return;

      // Ask the user what to find
      FindTextDialog find = new FindTextDialog(this);
      Dimension dlgSize = find.getPreferredSize();
      Dimension frmSize = this.getSize();
      Point loc = getLocation();
      find.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
      find.show();
      String text = find.getText();
      boolean matchcase = find.getMatchcase();

      // Find it and display the results
      if (text != null && text.length() > 0)
         statusFrame.findText(findOpenKB().getKB(), (KBTreeFrame)of.getFrame(), text, matchcase);
   }

   // View | Connections
   void menuViewConnections_actionPerformed(ActionEvent e)
   {
      if (findOpenKB() == null) return;

//      Rectangle r = findOpenKB().getFrame().getBounds();
//      XRefFrame xref = new XRefFrame(this, findOpenKB().getKB(), r.x+r.width, r.y);
//      objectList.addElement(xref);
//      JDesktopPane desktop = this.getDesktopPane();
//      desktop.add(xref);
//      desktop.getDesktopManager().activateFrame(xref);
   }

   // View Problems
   void menuViewProblems_actionPerformed(ActionEvent e)
   {
      if (findOpenKB() == null) return;

//      Rectangle r = findOpenKB().getFrame().getBounds();
//      IntegrityCheckerFrame intchk = new IntegrityCheckerFrame(this, findOpenKB().getKB(), r.x + r.width, r.y);
//      objectList.addElement(xref);
//      JDesktopPane desktop = this.getDesktopPane();
//      desktop.add(intchk);
//      desktop.getDesktopManager().activateFrame(intchk);
   }

   // Run | Run
   void menuRunRun_actionPerformed(ActionEvent e, boolean singleStep)
   {
      String runXPL;

      if (findOpenKB() == null) return;

      // Save the knowledgebase first if it's modified
      KB kb = findOpenKB().getKB();
      if (!saveOpenKB(kb)) return;

      // Then run it
      File tlb = new File(kb.getPath());
      if (tlb != null)
      {
         // Toggle the run menu
         toggleRunMenu(true);

         // Put execution control frame and log on the left half
         String logLevel = "low";
         if (menuLogLow.isSelected()) logLevel = "low";
         else if (menuLogMedium.isSelected()) logLevel = "medium";
         else if (menuLogHigh.isSelected()) logLevel = "high";
         else if (menuLogNone.isSelected()) logLevel = "none";

         // Determine the runtime XPL file
         try
         {
            runXPL = kb.getJig() + ".xpl";
         }
         catch (KBUserException ex)
         {
            JOptionPane.showMessageDialog(this, ex.getMessage(),
               ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
            return;
         }

         // Get the size of the app
         Dimension deskSize = desktopPane.getSize();

         // Create an html frame for solution display
         HTMLFrame html = new HTMLFrame(this, "Output", initialDir);
         if (getConfigEntry("run_out_w").equals("0") || getConfigEntry("run_out_h").equals("0"))
            html.setBounds(deskSize.width/2, 0, deskSize.width/2, deskSize.height);
         else
         {
            html.setBounds(new Integer(getConfigEntry("run_out_x")).intValue(), new Integer(getConfigEntry("run_out_y")).intValue(),
                new Integer(getConfigEntry("run_out_w")).intValue(),  new Integer(getConfigEntry("run_out_h")).intValue());
         }
         html.setClosable(false);
         html.setVisible(true);
         desktopPane.add(html);
         desktopPane.getDesktopManager().activateFrame(html);

         // Create the runtime frame
         KBRunFrame frame = new KBRunFrame(this, tlb, html, initialDir, runXPL, singleStep, logLevel, kb.getODBC());
         if (getConfigEntry("run_log_w").equals("0") || getConfigEntry("run_log_h").equals("0"))
            frame.setBounds(0, 0, deskSize.width/2, deskSize.height);
         else
         {
            frame.setBounds(new Integer(getConfigEntry("run_log_x")).intValue(), new Integer(getConfigEntry("run_log_y")).intValue(),
                new Integer(getConfigEntry("run_log_w")).intValue(),  new Integer(getConfigEntry("run_log_h")).intValue());
         }
         desktopPane.add(frame);
         desktopPane.getDesktopManager().activateFrame(frame);
      }
   }

   private boolean saveOpenKB(KB kb)
   {
      if (kb.isModified())
      {
         int value =  JOptionPane.showConfirmDialog(this, res.getString("SaveKBChanges.Prompt") +
            " " + kb.getName(), res.getString("menuClose.Text"), JOptionPane.YES_NO_OPTION) ;
         switch (value)
         {
            case JOptionPane.YES_OPTION:
               try
               {
                  kb.save();
               }
               catch (KBUserException ex)
               {
                  JOptionPane.showMessageDialog(this, ex.getMessage(),
                     ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
                  return false;
               }
               break;
            case JOptionPane.NO_OPTION:
            default:
               return false;
         }
      }
      return true;
   }

   // Run | Compile
   void menuRunCompile_actionPerformed(ActionEvent e)
   {
      OpenFile of;
      KB kb;

      // Only if registered
/*      if (!isRegistered())
      {
         JOptionPane.showMessageDialog(this, "This feature is only available to registered users. " +
            "E-mail info@amzi.com to learn how to unlock this product.",
            "Compile", JOptionPane.INFORMATION_MESSAGE);
         return;
      } */

      // Find the current KB, save it first, if needed
      of = findOpenKB();
      if (of == null) return;
      kb = of.getKB();
      if (!saveOpenKB(kb)) return;

      // Then try to compile
      startWait();
      try
      {
         kb.compile();
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }
      stopWait();

      JOptionPane.showMessageDialog(this, "Successfully compiled " + kb.getName(),
         "Compile", JOptionPane.INFORMATION_MESSAGE);

   }

   // Tools | Preferences
   void menuToolsPreferences_actionPerformed(ActionEvent e)
   {
      // Create the dialog box
      PreferencesDialog pref = new PreferencesDialog(this, settings);

      // Center it
      Dimension deskSize = desktopPane.getSize();
      Dimension prefSize = pref.getSize();
      pref.setLocation((deskSize.width-prefSize.width)/2, (deskSize.height-prefSize.height)/2);

      // And put it up
      pref.show();

      // Set the new locale
      if (!Locale.getDefault().toString().equals(getConfigEntry("locale")))
      {
         String loc = getConfigEntry("locale");
         String parts[] = parseLocale(loc);
         Locale.setDefault(new Locale(parts[0], parts[1], parts[2]));
      }
   }

   // Tools | Font
   void menuToolsFont_actionPerformed(ActionEvent e)
   {
      OpenFile of;

      // Create the dialog box
      FontChooser fontChooser = new FontChooser(this, "Select Font", true);
      fontChooser.setAllowAnyFontSize(false);
      if (defaultFont != null)
         fontChooser.setSelectedFont(defaultFont);

      // And put it up
      if (fontChooser.showDialog())
      {
         defaultFont = fontChooser.getSelectedFont();

         // Change the font in the currently open KB
         if (statusFrame != null) statusFrame.setTextFont(defaultFont);
         of = findOpenKB();
         if (of != null) ((KBTreeFrame)of.getFrame()).setTextFont(defaultFont);

         // Save the new font
         setConfigEntry("font_name", defaultFont.getFontName());
         setConfigEntry("font_style", new Integer(defaultFont.getStyle()).toString());
         setConfigEntry("font_size", new Integer(defaultFont.getSize()).toString());
      }
   }

   // Tools | Convert
   void menuToolsConvert_actionPerformed(ActionEvent e)
   {
      KB kb;
      boolean check;

      // Don't let another open or convert start
      toggleKBMenu(true);

      if (findOpenKB() != null)
      {
         JOptionPane.showMessageDialog(this, "Close current knowledgebase before converting",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         return;
      }

      // Show the user we're working
      startWait();

      // Get the old knowledgebase
      File f = selectSourceFile("Knowledgebase to Convert", "lb", JFileChooser.OPEN_DIALOG);
      if (f == null)
      {
         stopWait();
         return;
      }

      File fnew = selectSourceFile("New Knowledgebase", "kb", JFileChooser.SAVE_DIALOG);
      if (fnew == null)
      {
         stopWait();
         return;
      }

      if (f.getAbsolutePath().equals(fnew.getAbsolutePath()))
      {
         JOptionPane.showMessageDialog(this, "Cannot convert into same filename",
            "ERROR", JOptionPane.ERROR_MESSAGE);
         stopWait();
         return;
      }
      try
      {
         kb = new KB(this, f, fnew, editXPL);
         Dimension deskSize = desktopPane.getSize();

         // Put up the status tab pane
         statusFrame = new StatusTabFrame(this, kb);
         statusFrame.setBounds(0, deskSize.height*4/5, deskSize.width, deskSize.height*1/5);
         desktopPane.add(statusFrame);
//         desktopPane.getDesktopManager().activateFrame(statusFrame);

         // Do a status check if needed
         if (getConfigEntry("auto_update_status").equals("true"))
            statusFrame.updateDisplay(null, null);

         // Then the kb tree
         KBTreeFrame frame = new KBTreeFrame(this, statusFrame, kb, settings, docsDirectory);
         frame.setBounds(0, 0, deskSize.width*1/4, deskSize.height*4/5);
         desktopPane.add(frame);
//         desktopPane.getDesktopManager().activateFrame(frame);
         menuViewProblems_actionPerformed(null);
      }
      catch (KBUserException ex)
      {
         JOptionPane.showMessageDialog(this, ex.getMessage(),
            ex.getErrorType(), JOptionPane.ERROR_MESSAGE);
      }

      stopWait();
   }

   // Help | About
   private void menuHelpAbout_actionPerformed(ActionEvent e)
   {
      AboutBox dlg = new AboutBox(this, settings);
      Dimension dlgSize = dlg.getPreferredSize();
      Dimension frmSize = getSize();
      Point loc = getLocation();
      dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x, (frmSize.height - dlgSize.height) / 2 + loc.y);
      dlg.setModal(true);
      dlg.show();
   }

   // Help | Tutorial
   private void menuHelpTutorial_actionPerformed(ActionEvent e)
   {
      try
      {
         Utils.displayURL(getConfigEntry("web_browser"), "file://" + docsDirectory + "jigs" +
            System.getProperty("file.separator") + "jigs_tutorial.html");
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, "Unable to open web browser specified in Preferences to display help: " +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
         menuToolsPreferences_actionPerformed(null);
      }
   }

   // Help | License
   private void menuHelpLicense_actionPerformed(ActionEvent e)
   {
      LicenseDialog license = new LicenseDialog(this);
      license.show();
   }

   // Help | Amzi!
   private void menuHelpAmzi_actionPerformed(ActionEvent e)
   {
      try
      {
//         String s = getConfigEntry("web_browser") + " http://www.amzi.com";
//         Runtime.getRuntime().exec(s);
         Utils.displayURL(getConfigEntry("web_browser"), "http://www.amzi.com");
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, "Unable to open web browser specified in Preferences to display help: " +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
         menuToolsPreferences_actionPerformed(null);
      }
   }

   // Help | Support
   private void menuHelpSupport_actionPerformed(ActionEvent e)
   {
      try
      {
         Utils.displayURL(getConfigEntry("web_browser"), "http://www.amzi.com/support");
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, "Unable to open web browser specified in Preferences to display help: " +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
         menuToolsPreferences_actionPerformed(null);
      }
   }

   // Help | Support Forums
   private void menuHelpForums_actionPerformed(ActionEvent e)
   {
      try
      {
         Utils.displayURL(getConfigEntry("web_browser"), "http://www.amzi.com/phorum/index.php");
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, "Unable to open web browser specified in Preferences to display help: " +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
         menuToolsPreferences_actionPerformed(null);
      }
   }

   // Help | Contents
   private void menuHelpContents_actionPerformed(ActionEvent e)
   {
      try
      {
//         String s = getConfigEntry("web_browser") + " file://" + docsDirectory + "knowledgewright_manual.html";
//         Runtime.getRuntime().exec(s);
         Utils.displayURL(getConfigEntry("web_browser"), "file://" + docsDirectory + "knowledgewright_manual.html");
      }
      catch (Exception ex)
      {
         JOptionPane.showMessageDialog(this, "Unable to open web browser specified in Preferences to display help: " +
            "\n" + ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
         menuToolsPreferences_actionPerformed(null);
      }
   }

   protected void processWindowEvent(WindowEvent e)
   {
      if (e.getID() == WindowEvent.WINDOW_CLOSING)
      {
         if (okToClose(JOptionPane.YES_NO_CANCEL_OPTION))
         {
            super.processWindowEvent(e);
            System.exit(0);
         }
      }
      else
         super.processWindowEvent(e);
   }

   private void saveConfig()
   {
      // Try to save the location of the window for next time
      if (this.getState() == Frame.NORMAL)
      {
         Point p = this.getLocationOnScreen();
         setConfigEntry("screen_x", new Integer(p.x).toString());
         setConfigEntry("screen_y", new Integer(p.y).toString());
         setConfigEntry("screen_h", new Integer(this.getHeight()).toString());
         setConfigEntry("screen_w", new Integer(this.getWidth()).toString());
      }

      // And the reopen file list
      for (int i = 0 ; i < menuKBReopen.getItemCount() ; i++)
      {
         String istr = new Integer(i).toString();
         settings.set("open"+istr, menuKBReopen.getItem(i).getText());
      }
      try
      {
         settings.save();
      }
      catch (Exception ex)
      {
      }
   }

   private String[] jigList()
   {
      ExtFilenameFilter jigFilter = new ExtFilenameFilter("jig");
      File initialDirFile = new File(initialDir + "jigs");
      String jigs[] = initialDirFile.list(jigFilter);
      for (int i = 0 ; i < jigs.length ; i++)
         jigs[i] = jigs[i].substring(0,jigs[i].lastIndexOf('.'));
      return jigs;
   }

   private boolean okToClose(int option)
   {
      boolean retval = true;

      setConfigEntry("kb_open", "false");

      // Walk the list of opened files to see if they need saving
      for (int i = 0  ; i < openFileList.size() ; i ++)
      {
         OpenFile of  = (OpenFile)openFileList.elementAt(i);
//         if (of.getFrame().getClass() == classEditFrame)
//         {
//            if (((EditFrame)of.getFrame()).okToClose(option) == false)
//               retval = false;
//         }
//         else
         if (of.getFrame().getClass() == classKBTreeFrame)
         {
            closeKB(of);
            setConfigEntry("kb_open", "true");
         }
         else
         {
            JOptionPane.showMessageDialog(this, "Unrecognized Frame Class for Save: " + of.getFrame().getClass().getName(),
               "INTERNAL ERROR", JOptionPane.ERROR_MESSAGE);
            retval = false;
         }
      }

      saveConfig();

      return retval;
   }

   private void closeKB(OpenFile of)
   {
      // Close the tree first
      Rectangle r = of.getFrame().getBounds();
      setConfigEntry("tree_x", new Integer(r.x).toString());
      setConfigEntry("tree_y", new Integer(r.y).toString());
      setConfigEntry("tree_h", new Integer(r.height).toString());
      setConfigEntry("tree_w", new Integer(r.width).toString());

      ((KBTreeFrame)of.getFrame()).okToClose(JOptionPane.YES_NO_CANCEL_OPTION);
      desktopPane.getDesktopManager().closeFrame(of.getFrame());
      desktopPane.remove(of.getFrame());
      of.getFrame().dispose();
      removeOpenFile(of);

      // Then the status box
      r = statusFrame.getBounds();
      setConfigEntry("status_x", new Integer(r.x).toString());
      setConfigEntry("status_y", new Integer(r.y).toString());
      setConfigEntry("status_h", new Integer(r.height).toString());
      setConfigEntry("status_w", new Integer(r.width).toString());
      desktopPane.getDesktopManager().closeFrame(statusFrame);
      desktopPane.remove(statusFrame);
      statusFrame.dispose();

      toggleKBMenu(false);

      System.gc();
   }

   private void toggleKBMenu(boolean kbOpen)
   {
      if (kbOpen)
      {
         menuKBNew.setEnabled(false);
         menuKBOpen.setEnabled(false);
         menuKBReopen.setEnabled(false);
         menuKBSave.setEnabled(true);
         menuKBSaveAs.setEnabled(true);
         menuKBClose.setEnabled(true);
         menuKBUnlock.setEnabled(false);
//         menuKBPrint.setEnabled(true);
         menuSearchFind.setEnabled(true);
         menuRunRun.setEnabled(true);
         menuRunCompile.setEnabled(true);
         menuRunSingleStep.setEnabled(true);
         menuToolsConvert.setEnabled(false);
//         menuViewProblems.setEnabled(true);
//         menuViewConnections.setEnabled(true);
      }
      else
      {
         menuKBNew.setEnabled(true);
         menuKBOpen.setEnabled(true);
         menuKBReopen.setEnabled(true);
         menuKBSave.setEnabled(false);
         menuKBSaveAs.setEnabled(false);
         menuKBClose.setEnabled(false);
         menuKBUnlock.setEnabled(true);
//         menuKBPrint.setEnabled(false);
         menuSearchFind.setEnabled(false);
         menuRunRun.setEnabled(false);
         menuRunSingleStep.setEnabled(false);
         menuRunCompile.setEnabled(false);
         menuToolsConvert.setEnabled(true);
//         menuViewProblems.setEnabled(false);
//         menuViewConnections.setEnabled(false);
      }
   }


   public void toggleRunMenu(boolean running)
   {
      if (running)
      {
         menuRunRun.setEnabled(false);
         menuRunSingleStep.setEnabled(false);
         menuRunLogDetail.setEnabled(false);
         menuRunCompile.setEnabled(false);
      }
      else
      {
         menuRunRun.setEnabled(true);
         menuRunSingleStep.setEnabled(true);
         menuRunLogDetail.setEnabled(true);
         menuRunCompile.setEnabled(true);
      }
   }

//----------------------------------------------------------------------------//

   // Public Methods


   public void setConfigEntry(String name, String value)
   {
      settings.set(name, value);
   }

   public String getConfigEntry(String name)
   {
      return settings.get(name);
   }

   public void setKBModified(KB kb, boolean setting)
   {
      OpenFile of = findOpenKB();
      if (of != null)
         ((KBTreeFrame)of.getFrame()).setKBNameModified(setting);
   }

   public Font getUserFont()
   {
      return defaultFont;
   }

   public boolean isRegistered()
   {
      if (unregistered) return false;
      else return true;
   }

//----------------------------------------------------------------------------//

   // Open File List Maintenance

   public void addOpenFile(OpenFile of)
   {
      openFileList.add(of);
   }

   public boolean removeOpenFile(String path)
   {
      OpenFile of = findOpenFile(path);
      if (of != null)
         openFileList.removeElement(of);
      else
         return false;
      return true;
   }

   public boolean removeOpenFile(OpenFile of)
   {
      if (of != null)
         openFileList.remove(of);
      else
         return false;
      return true;
   }

   public boolean renameOpenFile(String oldPath, String newPath)
   {
      OpenFile of = findOpenFile(oldPath);
      if (of != null)
         of.setPath(newPath);
      else
         return false;
      return true;
   }

   public OpenFile findOpenFile(String path)
   {
      int i;
      for (i = 0  ; i < openFileList.size() ; i ++)
      {
         OpenFile of  = (OpenFile)openFileList.elementAt(i);
         if (of.getPath().equals(path))
            return (OpenFile)openFileList.elementAt(i);
      }
      return null;
   }

   public OpenFile findOpenKB()
   {
      int i;
      for (i = 0  ; i < openFileList.size() ; i ++)
      {
         OpenFile of  = (OpenFile)openFileList.elementAt(i);
         if (of.getKB() != null)
            return (OpenFile)openFileList.elementAt(i);
      }
      return null;
   }

/*   // Open a File in a New Frame
   public void openEditFile(File f)
   {
      // Put it in a nice place
      Dimension deskSize = desktopPane.getSize();
      EditFrame frame = new EditFrame(this, f, 0);
      frame.setBounds(lastEditXPos, lastEditYPos, deskSize.width*3/4, deskSize.height*3/4);
      lastEditXPos = lastEditXPos + deskSize.height/10;
      lastEditYPos = lastEditYPos + deskSize.height/10;
      desktopPane.add(frame);
      desktopPane.getDesktopManager().activateFrame(frame);
   }
*/

   public void startWait()
   {
      lastCursor = getCursor();
      setCursor(new Cursor(Cursor.WAIT_CURSOR));
   }

   public void stopWait()
   {
      setCursor(lastCursor);
   }

   public String[] parseLocale(String loc)
   {
      String[] parts = new String [3];
      String lang = "";
      String country = "";
      String variant = "";

      int u1 = loc.indexOf("_", 0);
      lang = loc.substring(0, u1);
      int u2 = loc.indexOf("_", u1+1);
      if (u2 == -1) u2 = loc.length();
      country = loc.substring(u1+1, u2);
      if (u2 < loc.length())
      {
         int u3 = loc.indexOf("_", u2+1);
         variant = loc.substring(u2+1);
      }
      parts[0] = lang;
      parts[1] = country;
      parts[2] = variant;
      return parts;
   }

   public JDesktopPane getDesktopPane()
   {
      return desktopPane;
   }

   public File selectSourceFile(String title, String suffix, int mode)
   {
      File f = null;
      try
      {
         Cursor old_curse = getCursor();
         setCursor(new Cursor(Cursor.WAIT_CURSOR));
         File path = new File( System.getProperty("user.dir") );
         fileChooser.setCurrentDirectory(path);
         fileChooser.setDialogType(mode);

         // Setup file filters
         fileChooser.resetChoosableFileFilters();
         ExtFileFilter kbFilter = new ExtFileFilter("kb", "Knowledgebase Files");
         ExtFileFilter lbFilter = new ExtFileFilter("lb", "Logicbase files");
         if (suffix.equals("kb"))
         {
            fileChooser.addChoosableFileFilter(kbFilter);
            fileChooser.setFileFilter(kbFilter);
         }
         if (suffix.equals("lb"))
         {
            fileChooser.addChoosableFileFilter(lbFilter);
            fileChooser.setFileFilter(lbFilter);
         }

         // Set title
         if (title != null) fileChooser.setDialogTitle(title);

         // Set path
         if (lastOpenPath != null) fileChooser.setCurrentDirectory(new File(lastOpenPath));

         setCursor(old_curse);
         if (mode == JFileChooser.OPEN_DIALOG)
         {
            if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION)
               f = fileChooser.getSelectedFile();
         }
         else
         {
            if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION)
            {
               f = fileChooser.getSelectedFile();
               if (!f.getPath().endsWith(suffix))
                  f = new File(f.getPath() + "." + suffix);
            }
         }
      }
      catch (Exception ex)
      {
            JOptionPane.showMessageDialog(this, ex.getMessage(),
               "INTERNAL ERROR", JOptionPane.ERROR_MESSAGE);
      }

      if (f != null) lastOpenPath = f.getAbsolutePath();

      return f;
   }

}
