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

public class AuthorPredicates
{
	private LogicServer ls;

   public AuthorPredicates(LogicServer ls) throws LSException
   {
      this.ls = ls;

      // This appears to not work in a jar file
      // Must use slashes (not dots) to delineate the package name, or poof!
      ls.AddPred("report", 1, "amzi/ide/AuthorPredicates", "report", this);
   }

	public boolean report()
	{
/*      try
      {
         System.out.println(ls.GetStrParm(1));
      }
      catch (LSException e)
      {
         System.out.println(e.GetMsg());
         return false;
      }
*/      return true;
   }

}
