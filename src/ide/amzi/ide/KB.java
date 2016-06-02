
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import amzi.ls.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

public class KB
{
   private Vector objects, folders;
   private LogicServer ls;
   private File kbFile;
   private boolean dirty = false;
   private boolean xrefDone = false;
   private String editXPL, schemaPRO;
   private App app;
   private String license_name, license_organization, license_serial_number, license_unlock_code;

   // Open existing
   public KB(App app, File file, String editXPL) throws KBUserException
   {
      this.app = app;
      this.editXPL = editXPL;
      openKB(file, editXPL);
   }

   // Convert and open
   public KB(App app, File file, File newFile, String editXPL) throws KBUserException
   {
      this.app = app;
      this.editXPL = editXPL;

      try
      {
         ls = new LogicServer();
         ls.Init(app.initialDir + "amzi.cfg");
//         AuthorPredicates ap = new AuthorPredicates(ls);
         ls.AddLSX("aosutils", 0);
         String xplPath = Utils.findClasspathFile(editXPL, "");
         ls.Load(xplPath);

         // Load the basic jig
         String schemaPRO = "basic.jig";
         String schemaPath = Utils.findClasspathFile(schemaPRO, "jigs");
         if (ls.ExecStr("consult('" + Utils.doubleSlashes(schemaPath) + "')") == 0)
            throw new KBUserException(KBUserException.CANNOT_LOAD_SCHEMA);

         // Convert it
         String s = "convert324($" + Utils.doubleSlashes(file.getAbsolutePath()) + "$, $" + Utils.doubleSlashes(newFile.getAbsolutePath()) + "$)";
         if (ls.ExecStr(s) == 0)
            throw new KBUserException(KBUserException.CANNOT_CONVERT, file.getAbsolutePath());
         ls.Close();

         // Then open it
         openKB(newFile, editXPL);

         // And mark it modified
         setModified(true);
      }
      catch (LSException ex)
      {
         throw new KBUserException("convert324", ex);
      }
   }

   // Create new
   public KB(App app, File file, String editXPL, String jig) throws KBUserException
   {
      String s;

      this.app = app;
      this.editXPL = editXPL;
      kbFile = file;
      setModified(false);
      try
      {
         ls = new LogicServer();
         ls.Init(app.initialDir + "amzi.cfg");
//         AuthorPredicates ap = new AuthorPredicates(ls);
         ls.AddLSX("aosutils", 0);
         String xplPath = Utils.findClasspathFile(editXPL, "");
         ls.Load(xplPath);
         if (ls.ExecStr("auth_init") == 0)
            throw new KBUserException(KBUserException.CANNOT_INIT);

         String schemaPRO = jig + ".jig";
         String schemaPath = Utils.findClasspathFile(schemaPRO, "jigs");
         if (ls.ExecStr("consult('" + Utils.doubleSlashes(schemaPath) + "')") == 0)
//         if (ls.ExecStr("consult('" + schemaPath + "')") == 0)
            throw new KBUserException(KBUserException.CANNOT_LOAD_SCHEMA);

         String path = kbFile.getParent() + System.getProperty("file.separator");
         String name = kbFile.getName();
         String jigPath = Utils.findClasspathFile("jigs", "") + System.getProperty("file.separator");
         s = "auth_new('" + Utils.doubleSlashes(path) + "', '" + Utils.doubleQuotes(name) + "', '" + jig + "', '" + Utils.doubleSlashes(jigPath) + "')";
//         s = "auth_new('" + path + "', '" + name + "', '" + jig + "', '" + jigPath + "')";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_CREATE, name);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_new", ex);
      }
   }

   private void openKB(File file, String editXPL) throws KBUserException
   {
      String s;
      long term;

      this.editXPL = editXPL;
      kbFile = file;
      setModified(false);
      try
      {
         ls = new LogicServer();
         ls.Init(app.initialDir + "amzi.cfg");
//         AuthorPredicates ap = new AuthorPredicates(ls);
         ls.AddLSX("aosutils", 0);

         String xplPath = Utils.findClasspathFile(editXPL, "");
         ls.Load(xplPath);
         if (ls.ExecStr("auth_init") == 0)
            throw new KBUserException(KBUserException.CANNOT_INIT);

         String name = kbFile.getPath();
         String jigPath = Utils.findClasspathFile("jigs", "") + System.getProperty("file.separator");

         // Check if it needs conversion
         s = "auth_open_check('" + Utils.doubleSlashes(name) + "', '" + Utils.doubleSlashes(jigPath) + "', _VAR_FILEVER, _VAR_JIGVER)";
         term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_OPEN, name);
         int fileVer = ls.GetIntArg(term, 3);
         int jigVer = ls.GetIntArg(term, 4);
         if (fileVer != jigVer)
         {
            if (JOptionPane.showConfirmDialog(app, "Knowledgebase was built with an older jig, OK to convert?",
               "Knowledgebase Open", JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
            {
               ls.Close();
               return;
            }
            else
            {
               setModified(true);
            }
         }
         s = "auth_open('" + Utils.doubleSlashes(name) + "', '" + Utils.doubleSlashes(jigPath) + "')";

         if (ls.ExecStr(s) == 0)
            throw new KBUserException(KBUserException.CANNOT_OPEN, name);
      }
      catch (LSException ex)
      {
         throw new KBUserException("kb / openKB / auth_open", ex);
      }
   }

//----------------------------------------------------------------------------//

   // Registration

   public boolean isRegistered()
   {
      long term;
      try
      {
         term = ls.ExecStr("register:unl$gri(_User, _Org, _Info, _Prod, _Key)");
         if (term == 0) return false;

         license_name = ls.GetStrArg(term, 1);
         license_organization = ls.GetStrArg(term, 2);
         license_serial_number = ls.GetStrArg(term, 4);
         license_unlock_code = ls.GetStrArg(term, 5);
         return true;
      }
      catch (Exception ex)
      {
         return false;
      }
   }

//----------------------------------------------------------------------------//

   // KB Attributes

   public boolean getODBC()
   {
      long term;
      String s;
      try
      {
         term = ls.ExecStr("auth_get_ids(_, 'knowledgebase', _VAR_X)");
         if (term == 0) return false;
         long list = ls.GetArg(term, 3);
         Vector v = Utils.prologListToVector(ls, list, 1000);

         s = "auth_get_slot(_, 'knowledgebase', '" +
               Utils.doubleQuotes(((String)v.firstElement())) + "', 'odbc', _VAR_X)";
         term = ls.ExecStr(s);
         if (term == 0) return false;
         String db = ls.GetStrArg(term, 5);
         if (db != null && db.length() > 0) return true;
         return false;
      }
      catch (Exception ex)
      {
         return false;
      }
   }

   public String getHelpURL() throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_schema_global(help_url, _VAR_X)");
         if (term == 0) return "";
         return ls.GetStrArg(term, 2);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_schema_global", ex);
      }
   }

   public String getName()
   {
      return kbFile.getName();
   }

   public String getPath()
   {
      return kbFile.getPath();
   }

   public ImageIcon getIcon()
   {
      if (getClass().getResource("resources/ide_icon.gif") != null)
         return new ImageIcon((getClass().getResource("resources/ide_icon.gif")));
      else
         return null;
   }

   public LogicServer getLogicServer()
   {
      return ls;
   }

   public boolean isModified()
   {
      return dirty;
   }

   public void setModified(boolean state)
   {
      app.setKBModified(this, state);
      dirty = state;

      // Invalidate the cross reference when dirtied
      if (dirty) xrefDone = false;
   }

   public String getJig() throws KBUserException
   {
      try
      {
         String  s = "knowledgewright_jig(_VAR_NAME, _VAR_VERSION)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_FIND_JIG);
         return ls.GetStrArg(term, 1);
      }
      catch (LSException ex)
      {
         throw new KBUserException("knowledgewright_jig", ex);
      }
   }

//----------------------------------------------------------------------------//

   // KB Management

   public boolean save() throws KBUserException
   {
      try
      {
//         String path = Utils.doubleSlashes(kbFile.getParent() + System.getProperty("file.separator"));
//         String name = Utils.doubleSlashes(kbFile.getName());
         String path = kbFile.getParent() + System.getProperty("file.separator");
         String name = kbFile.getName();
         String  s = "auth_save('" + Utils.doubleSlashes(path) + "', '" + Utils.doubleSlashes(name) + "', " +
            app.getConfigEntry("unicode") + ")";
        long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_SAVE, name);
         setModified(false);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_save", ex);
      }
   }

   public boolean saveAs(String newPathname) throws KBUserException
   {
      try
      {
         String oldName = kbFile.getPath();
         File newFile = new File(newPathname);
         String newPath = newFile.getParent() + System.getProperty("file.separator");
         String newName = newFile.getName();
         String  s = "auth_save_as('" + Utils.doubleSlashes(oldName) + "', '" +
            Utils.doubleSlashes(newPath) + "', '" + Utils.doubleSlashes(newName) + "', " +
            app.getConfigEntry("unicode") + ")";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_SAVE_AS, newPathname);
         kbFile = new File(newPathname);
         setModified(false);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_save_as", ex);
      }
   }

   public void close() throws KBUserException
   {
      try
      {
         String name = Utils.doubleSlashes(kbFile.getPath());
         String  s = "auth_close('" + Utils.doubleQuotes(name) + "')";
         long term = ls.ExecStr(s);
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_close failed");
         ls.Close();
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_close", ex);
      }
   }

   public void compile() throws KBUserException
   {
      try
      {
         String kbPathname = Utils.doubleSlashes(kbFile.getPath());
         String kbcFilename = kbFile.getName().substring(0, kbFile.getName().lastIndexOf('.')) + ".kbc";
         String kbcPathname = Utils.doubleSlashes(kbFile.getParent() + System.getProperty("file.separator") + kbcFilename);
         String  s = "auth_compile('" + Utils.doubleQuotes(kbPathname) + "', '" + Utils.doubleQuotes(kbcPathname) + "')";
         long term = ls.ExecStr(s);
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_compile failed");
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_compile", ex);
      }
   }

//----------------------------------------------------------------------------//

   // KB-Wide Operations like Find & Replace

   public long findText(String text, boolean matchcase) throws KBUserException
   {
      long term;
      String s, matchtf;

      try
      {
         if (matchcase) matchtf = "true";
         else matchtf = "false";
         s = "auth_find($" + Utils.doubleDollars(text) + "$, " + matchtf + ", _VAR_LIST)";
         if ((term = ls.ExecStr(s)) == 0)
            return 0;
         else
            return ls.GetArg(term, 3);
      }
      catch (LSException ex)
      {
         throw new KBUserException("KB / findText / auth_find", ex);
      }
   }

   public long replaceObjectName(String oldName, String newName) throws KBUserException
   {
      long term;
      String s;

      try
      {
         s = "auth_replace_object_name('" + Utils.doubleQuotes(oldName) + "', '" +
            Utils.doubleQuotes(newName) + "', _VAR_LIST)";
         if ((term = ls.ExecStr(s)) == 0)
            return 0;
         else
         {
            setModified(true);
            return ls.GetArg(term, 3);
         }

      }
      catch (LSException ex)
      {
         throw new KBUserException("KB / replaceObjectName / auth_replace_object_name", ex);
      }
   }


//----------------------------------------------------------------------------//

   // Integrity Checking

   public String checkIntegrity() throws KBUserException
   {
      long term, list;
      Vector undefined, unused, circles, badvalues;
      String problems = "";
      int i;

      try
      {
         // First run the cross reference, if needed
         crossReference();

         // Get values that are referenced but not defined
         term = ls.ExecStr("auth_xref_bad_values(_VAR_LIST)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_xref_bad_values");
         list = ls.GetArg(term, 1);
         badvalues = Utils.prologListToVector(ls, list, 1000);
         if (badvalues.size() > 0)
            problems = problems + "The following values are used but are not defined in a question:" + "\n";
         for (i = 0 ; i < badvalues.size() ; i++)
            problems = problems + "  " + badvalues.elementAt(i) + "\n";

         // Get objects that are referenced but not defined
         term = ls.ExecStr("auth_xref_undefined(_VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_xref_undefined");
         list = ls.GetArg(term, 1);
         undefined = Utils.prologListToVector(ls, list, 1000);
         if (badvalues.size() > 0) problems = problems + "\n";
         if (undefined.size() > 0)
            problems = problems + "The following objects are used but do not exist:" + "\n";
         for (i = 0 ; i < undefined.size() ; i++)
            problems = problems + "  " + undefined.elementAt(i) + "\n";

         // Get objects that are defined but not used
         term = ls.ExecStr("auth_xref_unused(_VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_xref_unused");
         list = ls.GetArg(term, 1);
         unused = Utils.prologListToVector(ls, list, 1000);
         if (undefined.size() > 0) problems = problems + "\n";
         if (unused.size() > 0)
            problems = problems + "The following objects exist but are not used:" + "\n";
         for (i = 0 ; i < unused.size() ; i++)
            problems = problems + "  " + unused.elementAt(i) + "\n";

         // Get things that call themselves
         term = ls.ExecStr("auth_xref_circles(_VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_xref_circles");
         list = ls.GetArg(term, 1);
         circles = Utils.prologListToVector(ls, list, 1000);
         if (unused.size() > 0) problems = problems + "\n";
         if (circles.size() > 0)
            problems = problems + "The following rules and/or tables have infinite loops:" + "\n";
         for (i = 0 ; i < circles.size() ; i++)
            problems = problems + "  " + circles.elementAt(i) + "\n";

         return problems;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_xref_*", ex);
      }
   }

   public void crossReference() throws KBUserException
   {
      if (xrefDone) return;
      try
      {
         long term = ls.ExecStr("auth_xref");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_xref");
         xrefDone = true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_xref", ex);
      }
   }

//----------------------------------------------------------------------------//

   // Object Management

   public String findObject(String name) throws KBUserException
   {
      long term;

      try
      {
         String s = "auth_get_object_class(_VAR_PATH, '" + Utils.doubleQuotes(name) + "', _VAR_CLASS)";
         if ((term = ls.ExecStr(s)) == 0)
            return null;
         else
         {
            if (ls.GetStrArg(term, 3).equals("unknown")) return null;
            String path = ls.GetStrArg(term, 1);
            if (path.equals("/"))
               return path + name;
            else
               return ls.GetStrArg(term, 1) + " / " + name;
         }
      }
      catch (LSException ex)
      {
         throw new KBUserException("findObject / auth_get_object_class", ex);
      }
   }

   public Vector getObjectTypes() throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_classes(_VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_classes failed");
         long list = ls.GetArg(term, 1);
         objects = Utils.prologListToVector(ls, list, 1000);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_classes", ex);
      }
      return objects;
   }

   public Vector getObjectNames(String type) throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_ids(_, '" + type + "', _VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_ids failed");
         long list = ls.GetArg(term, 3);
         Vector v = Utils.prologListToVector(ls, list, 1000);
         return v;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_ids", ex);
      }
   }

   public Vector getObjectPaths(String type) throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_paths(_, '" + type + "', _VAR_X)");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_paths failed");
         long list = ls.GetArg(term, 3);
         Vector v = Utils.prologListToVector(ls, list, 1000);
         Vector w = Utils.formatPaths(v);
         return w;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_ids", ex);
      }
   }

//----------------------------------------------------------------------------//

   // Folder Management

   public boolean isValidFolderName(String name)
   {
//      if (name.indexOf(' ') >= 0) return false;
      if (name.indexOf('/') >= 0) return false;
//      if (name.indexOf('\'') >= 0) return false;
//      char first = name.charAt(0);
//      if ((first < 'a' || first > 'z') && first != '_') return false;
      return true;
   }

   public boolean createFolder(String parent, String name) throws KBUserException
   {
      long term;

      try
      {
         String s = "auth_get_object_class(" + Utils.quotePathname(parent) + ", '" +
            Utils.doubleQuotes(name) + "', _VAR_CLASS)";
         term = ls.ExecStr(s);
         if (term != 0)
            if (parent.endsWith("/"))
               throw new KBUserException(KBUserException.DUPLICATE_NAME, parent + name);
            else
               throw new KBUserException(KBUserException.DUPLICATE_NAME, parent + " / " + name);

         term = ls.ExecStr("auth_folder_new(" + Utils.quotePathname(parent) + ", '" +
            Utils.doubleQuotes(name) + "')");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_folder_new failed");

         setModified(true);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_folder_new", ex);
      }
   }

   public boolean renameFolder(String oldPathname, String newName) throws KBUserException
   {
      long term;
      try
      {
         term = ls.ExecStr("auth_get_object_class(" + Utils.quotePathname(oldPathname) + ", '" +
            Utils.doubleQuotes(newName) + "', _VAR_CLASS)");
         if (term != 0)
            throw new KBUserException(KBUserException.DUPLICATE_NAME, newName);

         term = ls.ExecStr("auth_folder_rename(" + Utils.quotePathname(oldPathname) + ", '" + Utils.doubleQuotes(newName) + "')");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_folder_rename failed");

         setModified(true);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_folder_rename", ex);
      }
   }

   public boolean deleteFolder(String pathname) throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_folder_delete(" + Utils.quotePathname(pathname) + ")");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_folder_delete failed");

         setModified(true);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_folder_delete", ex);
      }
   }

   public boolean moveFolder(String oldPathname, String newPath) throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_folder_move(" + Utils.quotePathname(oldPathname) + ", " +
            Utils.quotePathname(newPath) + ")");
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_folder_move failed");

         setModified(true);
         return true;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_folder_move", ex);
      }
   }

   public Vector getRootFolders() throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_root_folders(_VAR_X)");
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_root_folders failed");
         long list = ls.GetArg(term, 1);
         Vector v = Utils.prologListToVector(ls, list, 1000);
         return v;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_root_folders", ex);
      }
   }

   public Vector getFolderFolders(String folder) throws KBUserException
   {
      try
      {
         // Get the folders
         String s = "auth_get_subfolders(" + Utils.quotePathname(folder) + ", _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_subfolders failed");
         long list = ls.GetArg(term, 2);
         Vector v = Utils.prologListToVector(ls, list, 1000);

         return v;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_subfolders", ex);
      }
   }

   public Vector getFolderObjects(String folder) throws KBUserException
   {
      try
      {
         // Get the contents
         long term = ls.ExecStr("auth_get_folder_contents(" + Utils.quotePathname(folder) + ", _VAR_X)");
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_folder_contents failed");
         long list = ls.GetArg(term, 2);
         Vector v = Utils.prologListToVector(ls, list, 1000);

         return v;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_folder_contents", ex);
      }
   }

   public Vector getRootObjects() throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_folderless_objects(_VAR_X)");
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_folderless_objects failed");
         long list = ls.GetArg(term, 1);
         Vector v = Utils.prologListToVector(ls, list, 1000);
         return v;
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_folderless_objects", ex);
      }
   }

   public Vector getAllFolderPaths() throws KBUserException
   {
      try
      {
         long term = ls.ExecStr("auth_get_folderpaths(_VAR_X)");
         if (term == 0)
               throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_folderpaths failed");
         long list = ls.GetArg(term, 1);
         Vector v = Utils.prologListToVector(ls, list, 1000);
         return Utils.formatPaths(v);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_get_folderpaths", ex);
      }
   }

   }