
//Title:        Amzi! IDE
//Version:
//Copyright:    Copyright (c) 1999
//Author:       Mary
//Company:      Amzi!
//Description:

package amzi.ide;

import amzi.ls.*;
import java.util.*;
import javax.swing.*;

public class KBObject
{
   // Add here and in getSlotDisplayType and getSlotValueType
   public static final int UNKNOWN = 0, TEXT_LINE = 1,
      TEXT_BOX = 3, MENU = 4, LIST_OBJECT = 5, TABLE = 6, TEXT_TABLE = 7, ID = 9,
      NAME_LIST = 10, FILE_PATH = 11,
      ATOM = 100, STRING = 101, NUMBER = 102, TERM = 103,
      LIST_OF_ATOMS = 104, LIST_OF_STRINGS = 105, LIST_OF_NUMBERS = 106, LIST_OF_TERMS = 107,
      RULES = 108;
   public static final int TERM_SIZE = 4000;

   private static Class classVector;
   String type, name, path, value_type;
   KB kb;
   LogicServer ls;

   // Create a new Java and KB object
   public KBObject(KB kb, String name, String type) throws KBUserException
   {
      this.kb = kb;
      this.name = name;
      this.type = type;
      ls = kb.getLogicServer();

      // Set up a variable for the vector class
      try
      {
         classVector = Class.forName("java.util.Vector");
      }
      catch (Exception e)
      {
         throw new KBUserException(KBUserException.NO_JAVA_CLASS_NAME, "java.util.Vector");
      }

      // Create the object
      try
      {
         this.path = "/";
         String s = "auth_new_object(/, '" + type + "', '" + Utils.doubleQuotes(name) + "')";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_CREATE_OBJECT, type + " named: " + name + " (already exists)");
         kb.setModified(true);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_new_object", ex);
      }
   }

   // Create a new Java and KB object from an existing object
   public KBObject(KB kb, String oldPath, String oldName, String type, String newName) throws KBUserException
   {
      this.kb = kb;
      this.name = newName;
      this.type = type;
      this.path = "/";
      ls = kb.getLogicServer();

      // Set up a variable for the vector class
      try
      {
         classVector = Class.forName("java.util.Vector");
      }
      catch (Exception e)
      {
         throw new KBUserException(KBUserException.NO_JAVA_CLASS_NAME, "java.util.Vector");
      }

      // Create the object
      try
      {
         // New object is in the root folder otherwise AddObject in KBTreeFrame will not work
         String s = "auth_copy_object(" + Utils.quotePathname(oldPath) + ", '" + type + "', '" +
            Utils.doubleQuotes(newName) + "', '" + Utils.doubleQuotes(oldName) + "')";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_CREATE_OBJECT, type + " named: " + newName + " (already exists)");
         kb.setModified(true);
      }
      catch (LSException ex)
      {
         throw new KBUserException("auth_copy_object", ex);
      }
   }

   // Create a Java object for an existing KB object
   public KBObject(KB kb, String pathname) throws KBUserException
   {
      String s;
      long term;

      this.kb = kb;
      ls = kb.getLogicServer();

      // Set up a variable for the vector class
      try
      {
         classVector = Class.forName("java.util.Vector");
      }
      catch (Exception e)
      {
         throw new KBUserException(KBUserException.NO_JAVA_CLASS_NAME, "java.util.Vector");
      }

      // Get the type
      try
      {
         s = "auth_split_path(" + Utils.quotePathname(pathname) + ", _VAR_PATH, _VAR_NAME)";
         if ((term = ls.ExecStr(s)) == 0)
            throw new KBUserException(KBUserException.CANNOT_OPEN_OBJECT, pathname);
         this.path = ls.TermToStr(ls.GetArg(term, 2), 1000);
         this.name = ls.TermToStr(ls.GetArg(term, 3), 1000);

         s = "auth_get_object_class(" + Utils.quotePathname(path) + ", '" + Utils.doubleQuotes(name) + "', _VAR_X)";
         if ((term = ls.ExecStr(s)) == 0)
            throw new KBUserException(KBUserException.CANNOT_OPEN_OBJECT, pathname);
         else
         {
            String objType = ls.TermToStr(ls.GetArg(term, 3), TERM_SIZE);
            this.type = objType;
         }
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_split_path or auth_get_object_class", ex);
      }
   }

//----------------------------------------------------------------------------//

   // Get Object-Level Attributes

   public KB getKB()
   {
      return kb;
   }

   public String getName()
   {
      return name;
   }

   public String getPath()
   {
      return path;
   }

   public String getPathname()
   {
      if (path.trim().equals("/"))
         return "/ " + name;
      else
         return path + " / " + name;
   }

   public String getType()
   {
      return type;
   }

   // Used in the tree display
   public String toString()
   {
      return name;
   }

   public static boolean isValidObjectName(String name)
   {
      if (name.indexOf(' ') >= 0 && (name.startsWith("'") == false || name.endsWith("'") == false))
         return false;
      if (name.indexOf('/') >= 0) return false;
      String middle = name.substring(1, name.length()-1);
      if (middle.indexOf('\'') > 0) return false;
      if (name.charAt(0) == '\'')
         return true;
      else
      {
         for (int i = 0 ; i < name.length() ; i++)
            if (!Character.isLetterOrDigit(name.charAt(i)) && name.charAt(i) != '_') return false;
      }
      return true;
   }

//----------------------------------------------------------------------------//

   // Get/Set Logicbase Slots

   public String getTextSlot(String slot) throws KBUserException
   {
      String value;

      try
      {
         // Get the type first so we don't interfere with ExecStr
         int value_type = getSlotValueType(slot);

         String s = "auth_get_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot + "', _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);
         if (value_type == TERM)
            value = ls.TermToStrQ(ls.GetArg(term, 5), TERM_SIZE);
         else
            value = ls.TermToStr(ls.GetArg(term, 5), TERM_SIZE);
         return value;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_slot", ex);
      }
   }

   public Vector getListSlot(String slot) throws KBUserException
   {
      Vector v;
      long list;

      try
      {
         // Get the type first so we don't interfere with ExecStr
         int value_type = getSlotValueType(slot);

         String s = "auth_get_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot + "', _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);
         if (ls.GetArgType(term, 5) != LogicServer.pLIST)
         {
            v = new Vector(0);
//            v.add(ls.TermToStr(ls.GetArg(term, 5), TERM_SIZE));
         }
         else
         {
            list = ls.GetArg(term, 5);
            if (value_type == LIST_OF_TERMS)
               v = Utils.prologListToVectorQ(ls, list, TERM_SIZE);
            else
               v = Utils.prologListToVector(ls, list, TERM_SIZE);
         }
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_slot", ex);
      }
   }

/*   public Vector getRuleSlot(String slot) throws KBUserException
   {
      Vector v;
      long list;

      try
      {
         String s = "auth_get_rule_slot(" + path + ", '" + type + "', '" + name + "', '" + slot + "', X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);
         if (ls.GetArgType(term, 5) != LogicServer.pLIST)
            v = new Vector(0);
         else
         {
            list = ls.GetArg(term, 5);
            v = Utils.prologListToVector(ls, list, TERM_SIZE);
         }
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_rule_slot", ex);
      }
   }
*/
   public Vector getTableRows(String slot) throws KBUserException
   {
      long list, listlist;
      Vector r, v = new Vector();

      try
      {
         // Get the type first so we don't interfere with ExecStr
         int value_type = getSlotValueType(slot);

         String s = "auth_get_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot + "', _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);

         // If it's not a list it must be empty
         if (ls.GetArgType(term, 5) != LogicServer.pLIST)
            return v;

         listlist = ls.GetArg(term, 5);
         list = ls.GetHead(listlist);
         listlist = ls.GetTail(listlist);
         while (listlist != 0)
         {
            list = ls.GetHead(listlist);
            if (value_type == LIST_OF_TERMS)
               r = Utils.prologListToVectorQ(ls, list, TERM_SIZE);
            else
               r = Utils.prologListToVector(ls, list, TERM_SIZE);
            v.addElement(r);
            listlist = ls.GetTail(listlist);
         }
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_slot", ex);
      }
   }

   public Vector getTableColumnNames(String slot) throws KBUserException
   {
      long list, listlist;
      Vector v = new Vector();

      try
      {
         String s = "auth_get_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot + "', _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);

         // If it's not a list it must be empty
         if (ls.GetArgType(term, 5) != LogicServer.pLIST)
            return v;

         listlist = ls.GetArg(term, 5);
         list = ls.GetHead(listlist);
         v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_slot", ex);
      }
   }

   public boolean setSlot(String slot, String value) throws KBUserException
   {
      try
      {
         String s = "auth_set_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot;
         switch (this.getSlotValueType(slot))
         {
            case STRING:
               s = s + "', $" +  Utils.doubleDollars(value) + "$)";
               break;
            case ATOM:
               s = s + "', '" + Utils.doubleQuotes(value) + "')";
               break;
            case NUMBER:
               if (value.length() == 0)
               {
                  s = s + "', 0)";
                  break;
               }
               checkSlotValue(slot, value);
            case TERM:
               if (value.length() == 0)
               {
                  s = s + "', '')";
                  break;
               }
/*            case RULES:
               if (value.length() == 0)
               {
                  s = s + "', [])";
                  break;
               }*/
            default:
               checkSlotValue(slot, value);
               s = s + "', " + value + ")";
               break;
         }

         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_SET_SLOT, slot + " = " + value);
         kb.setModified(true);
         return true;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_set_slot", ex);
      }
      catch (KBUserException ex2)
      {
         throw ex2;
      }
   }

   public boolean setSlot(String slot, Vector values) throws KBUserException
   {
      String vstr, s;

      // Build a moby string with all the values, possibly a vector of vectors
      vstr = "[";
      for (int i = 0 ; i < values.size() ; i ++)
      {
         if (i != 0) vstr = vstr + ", ";

         // Check if we have a vector of vectors
         if (values.elementAt(i).getClass() == classVector)
         {
            Vector values2 = (Vector)values.elementAt(i);
            vstr = vstr + "[";

            // If so, get all the values in that vector into their own list
            for (int j = 0 ; j < values2.size() ; j++)
            {
               if (j != 0) vstr = vstr + ", ";
               s = values2.elementAt(j).toString();
               switch (this.getSlotValueType(slot))
               {
                  case LIST_OF_STRINGS:
                     vstr = vstr + "$" + Utils.doubleDollars(s) + "$";
                     break;
                  case LIST_OF_ATOMS:
                     vstr = vstr + "'" + Utils.doubleQuotes(s) + "'";
                     break;
                  case LIST_OF_NUMBERS:
                  case LIST_OF_TERMS:
                  default:
                     if (s.length() == 0)
                        throw new KBUserException(KBUserException.EMPTY_TERM_SLOT_VALUE, slot +
                        " row " + Integer.toString(i) + " col " + Integer.toString(j+1));
                     checkSlotValue(slot, s);
                     vstr = vstr + s;
                     break;
               }
            }

            vstr = vstr + "]";
         }
         // Otherwise just get the values into the list
         else
         {
            s = values.elementAt(i).toString();
            switch (this.getSlotValueType(slot))
            {
               case LIST_OF_STRINGS:
                  vstr = vstr + "$" + Utils.doubleDollars(s) + "$";
                  break;
               case LIST_OF_ATOMS:
                  vstr = vstr + "'" + Utils.doubleQuotes(s) + "'";
                  break;
               case LIST_OF_NUMBERS:
               case LIST_OF_TERMS:
               default:
                  checkSlotValue(slot, s);
                  vstr = vstr + s;
                  break;
            }
         }
      }
      vstr = vstr + "]";

      try
      {
         s = "auth_set_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot +
            "', " + vstr + ")";;
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_SET_SLOT, slot + " = " + values.toString());
         kb.setModified(true);
         return true;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_set_slot", ex);
      }

   }

//----------------------------------------------------------------------------//

   // Check New Slot Values and Cross Reference

   public void checkSlotValue(String slot, String value) throws KBUserException
   {
      try
      {
         String s = "auth_check_slot(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + slot + "', \"" + Utils.doubleDoubleQuotes(value) + "\", _VAR_ERROR_LIST)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);

         // If we didn't get the empty list, there's a problem
         if (ls.GetArgType(term, 6) != LogicServer.pLIST)
            return;
         throw new KBUserException(Utils.prologListToProperties(ls, ls.GetArg(term, 6), 10000));
      }
      catch (LSException ex)
      {
          throw new KBUserException("checkSlotValue", ex);
      }
   }

   public String getUses() throws KBUserException
   {
      String text = "";
      try
      {
         // Run a cross reference, if needed
         kb.crossReference();

         String s = "auth_xref_uses('" + type + "', '" + Utils.doubleQuotes(name) + "', _VAR_LIST)";
         long term = ls.ExecStr(s);
         if (term == 0) return text;
         Vector v = Utils.prologListToVector(ls, ls.GetArg(term, 3), 10000);
         for (int i = 0 ; i < v.size() ; i++)
            text = text + v.elementAt(i) + "\n";
         if (text.length() == 0) text = "<none>";
         return text;
      }
      catch (LSException ex)
      {
          throw new KBUserException("getUses / auth_xref_uses", ex);
      }
   }

   public String getUsedBy() throws KBUserException
   {
      String text = "";
      try
      {
         // Run a cross reference, if needed
         kb.crossReference();

         String s = "auth_xref_usedby('" + type + "', '" + Utils.doubleQuotes(name) + "', _VAR_LIST)";
         long term = ls.ExecStr(s);
         if (term == 0) return text;
         Vector v = Utils.prologListToVector(ls, ls.GetArg(term, 3), 10000);
         for (int i = 0 ; i < v.size() ; i++)
            text = text + v.elementAt(i) + "\n";
         if (text.length() == 0) text = "<none>";
         return text;
      }
      catch (LSException ex)
      {
          throw new KBUserException("getUses / auth_xref_usedby", ex);
      }
   }

//----------------------------------------------------------------------------//

   // Get Schema Information about Slots

   public String getHelpURL() throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', 'id', help_url, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            return null;
         return ls.GetStrArg(term, 4);
      }
      catch (LSException ex)
      {
          throw new KBUserException("getHelpURL / auth_schema_slot_facet", ex);
      }
   }

   public int getSlotDisplayType(String slot) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', '" + slot + "', display, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT_TYPE, slot);
         String value = ls.GetStrArg(term, 4);
         if (value.equals("text_line")) return TEXT_LINE;
         if (value.equals("file_path")) return FILE_PATH;
         if (value.equals("text_box")) return TEXT_BOX;
         if (value.equals("menu")) return MENU;
         if (value.equals("list_object")) return LIST_OBJECT;
         if (value.equals("table")) return TABLE;
         if (value.equals("text_table")) return TEXT_TABLE;
         if (value.equals("id")) return ID;
         if (value.equals("name_list")) return NAME_LIST;
         return UNKNOWN;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }
   }

   public int getSlotValueType(String slot) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', '" + slot + "', type, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT_TYPE, slot);
         String value = ls.GetStrArg(term, 4);
         if (value.equals("atom")) return ATOM;
         if (value.equals("string")) return STRING;
         if (value.equals("number")) return NUMBER;
         if (value.equals("term")) return TERM;
         if (value.equals("list_of_atoms")) return LIST_OF_ATOMS;
         if (value.equals("list_of_strings")) return LIST_OF_STRINGS;
         if (value.equals("list_of_numbers")) return LIST_OF_NUMBERS;
         if (value.equals("list_of_terms")) return LIST_OF_TERMS;
         if (value.equals("rules")) return RULES;
         return UNKNOWN;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }
   }

   public Vector getSlotNames() throws KBUserException
   {
      try
      {
         String s = "auth_get_schema_slots('" + type + "', _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.PREDICATE_FAILED, "auth_get_schema_slots");
         long list = ls.GetArg(term, 2);
         Vector v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_get_schema_slots", ex);
      }
   }

   public String getSlotHelp(String slot) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', '" + slot + "', help, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            return null;
         String value = ls.GetStrArg(term, 4);
         return value;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }
   }

   public Vector getMenuValues(String slot) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', '" + slot + "', choices, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term == 0)
            throw new KBUserException(KBUserException.CANNOT_GET_SLOT, slot);
         long list = ls.GetArg(term, 4);
         Vector v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }
   }

   public Vector getMenuRelatedSlots(String slot, String choice) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_related('" + type + "', '" + slot + "', related_slots, '" + choice + "', _VAR_ON, _)";
         long term = ls.ExecStr(s);
         if (term == 0)
            return null;
         long list = ls.GetArg(term, 5);
         Vector v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_related", ex);
      }
   }

   public Vector getMenuUnrelatedSlots(String slot, String choice) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_related('" + type + "', '" + slot + "', related_slots, '" + choice + "', _, _VAR_OFF)";
         long term = ls.ExecStr(s);
         if (term == 0)
            return null;
         long list = ls.GetArg(term, 6);
         Vector v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_related", ex);
      }
   }

   public Vector getSlotListChoices(String slot) throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_list_choices('" + type + "', '" + slot + "', _VAR_LIST)";
         long term = ls.ExecStr(s);
         if (term == 0)
            return null;
         long list = ls.GetArg(term, 3);
         Vector v = Utils.prologListToVector(ls, list, TERM_SIZE);
         return v;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_list_choices", ex);
      }
   }

   public ImageIcon getIcon() throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', 'id', icon, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term != 0)
         {
            String value = ls.GetStrArg(term, 4);
            if (getClass().getResource("resources/"+value) != null)
               return new ImageIcon((getClass().getResource("resources/"+value)));
         }
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }

      return new ImageIcon((getClass().getResource("resources/default_icon.gif")));
   }

   public String getObjectHelp() throws KBUserException
   {
      try
      {
         String s = "auth_schema_slot_facet('" + type + "', 'id', object_help, _VAR_X)";
         long term = ls.ExecStr(s);
         if (term != 0)
         {
            String value = ls.GetStrArg(term, 4);
            return value;
         }
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_schema_slot_facet", ex);
      }

      return null;
   }

//----------------------------------------------------------------------------//

   // Get/Set Object-Level Attributes

   public boolean rename(String newName) throws KBUserException
   {
      try
      {
         String s = "auth_rename_object(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', '" + Utils.doubleQuotes(newName) + "')";;
         if (ls.ExecStr(s) == 0)
            throw new KBUserException(KBUserException.CANNOT_RENAME_OBJECT, name + " to " + newName);
         this.name = newName;
         kb.setModified(true);
         return true;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_rename_object", ex);
      }
   }

   // Delete the object
   public boolean delete() throws KBUserException
   {
      try
      {
         String s = "auth_delete_object(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "')";;
         if (ls.ExecStr(s) == 0)
            throw new KBUserException(KBUserException.CANNOT_DELETE_OBJECT, name);
         kb.setModified(true);
         return true;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_delete_object", ex);
      }
   }

   public boolean move(String folder) throws KBUserException
   {
      try
      {
         String s = "auth_move_object(" + Utils.quotePathname(path) + ", '" + type + "', '" +
            Utils.doubleQuotes(name) + "', " + Utils.quotePathname(folder) + ")";;
         if (ls.ExecStr(s) == 0)
            throw new KBUserException(KBUserException.CANNOT_MOVE_OBJECT, name + " to " + folder);
         this.path = folder;
         kb.setModified(true);
         return true;
      }
      catch (LSException ex)
      {
          throw new KBUserException("auth_move_object", ex);
      }
   }

}

