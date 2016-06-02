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
import java.awt.event.*;
import java.net.URL;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.html.*;

public class LicenseDialog extends JDialog
{
   JPanel formPanel = new JPanel();
   JEditorPane htmlLicense = new JEditorPane();
   JLabel prompt;
   JButton yesButton = new JButton();
   JButton noButton = new JButton();
   JPanel buttonPanel = new JPanel();
   JScrollPane htmlScroller;
   boolean accepted = false;
   App app;

   public LicenseDialog(App app)
   {
      super(app, "KnowledgeWright License", true);
      this.app = app;
      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      try
      {
         jbInit();
         URL licenseURL = new URL("file:" + app.docsDirectory + "license.html");
         htmlLicense.setPage(licenseURL);
         validate();
         this.setBounds(0, 0, screenSize.width/2, screenSize.height*3/4);
      }
      catch(Exception ex)
      {
         htmlLicense.setText("ERROR: Unable to locate license.html in " + app.docsDirectory);
         this.setBounds(0, 0, screenSize.width/2, screenSize.height*3/4);
      }
   }

   void jbInit() throws Exception
   {
      htmlLicense.setContentType("text/html");
      htmlLicense.getEditorKit().createDefaultDocument();
      htmlLicense.setEditable(false);
      htmlScroller = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      htmlScroller.getViewport().add(htmlLicense, null);
      htmlScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);

      yesButton.setText("Yes");
      yesButton.setMnemonic(KeyEvent.VK_Y);
      yesButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            yesButton_actionPerformed(e);
         }
      });
      noButton.setText("No");
      noButton.setMnemonic(KeyEvent.VK_N);
      noButton.addActionListener(new java.awt.event.ActionListener()
      {
         public void actionPerformed(ActionEvent e)
         {
            noButton_actionPerformed(e);
         }
      });
      prompt = new JLabel("Do you accept the above license terms?");
      prompt.setFont(app.getUserFont());
      buttonPanel.setLayout(new FlowLayout());
      buttonPanel.add(prompt, null);
      buttonPanel.add(yesButton, null);
      buttonPanel.add(noButton, null);

      formPanel.setLayout(new BorderLayout());
      formPanel.add(buttonPanel, BorderLayout.SOUTH);
      formPanel.add(htmlScroller, BorderLayout.CENTER);
      formPanel.setBorder(BorderFactory.createRaisedBevelBorder());

//      getContentPane().setBorder(BorderFactory.createRaisedBevelBorder());
      getContentPane().add(formPanel, BorderLayout.CENTER);
//      getContentPane().add(buttonPanel, BorderLayout.EAST);
   }

   void yesButton_actionPerformed(ActionEvent e)
   {
      accepted = true;
      this.hide();
   }

   void noButton_actionPerformed(ActionEvent e)
   {
      accepted = false;
      this.hide();
   }

   public boolean getAccepted()
   {
      return accepted;
   }

}