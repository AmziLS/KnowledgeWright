
//Title:      Amzi! IDE
//Version:
//Copyright:  Copyright (c) 1999
//Author:     Mary
//Company:    Amzi!
//Description:
package amzi.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.util.*;
//import com.borland.jbcl.control.*;
import com.borland.dbswing.ColumnLayout;

public class AboutBox extends JDialog implements ActionListener
{
   ResourceBundle res = ResourceBundle.getBundle("amzi.ide.resources.IDE");
   JPanel mainPanel = new JPanel();
   JPanel buttonPanel = new JPanel();
   JPanel textPanel = new JPanel();
   JButton okButton = new JButton();
   JLabel productTitle = new JLabel();
   JLabel copyrightLabel1 = new JLabel();
   BorderLayout borderLayout = new BorderLayout();
   String product = res.getString("ApplicationTitle.Text");
   JLabel copyrightLabel2 = new JLabel();
   JLabel urlLabel = new JLabel();
   ColumnLayout columnLayout = new ColumnLayout();
   JTextArea infoTextArea = new JTextArea();
   JScrollPane infoScrollPane = new JScrollPane(infoTextArea);

   public AboutBox(Frame parent, SavedSettings settings)
   {
      super(parent);
      enableEvents(AWTEvent.WINDOW_EVENT_MASK);
      try
      {
         String info;

         jbInit();
         info = "";
         if (settings.get("license_type").equals("professional") || settings.get("license_type").equals("standard"))
            info = info + "Licensed User: " + settings.get("license_name") + "\n" +
               "Organization: " + settings.get("license_organization") + "\n" +
               "Serial Number: " + settings.get("serial_number") + "\n";
//               "Unlock Code: " + settings.get("unlock_code") + "\n";
         info = info + "O.S. User: " + System.getProperty("user.name") + "\n" +
               "Operating System: " + System.getProperty("os.name") + "\n" +
               "O.S. Version: " + System.getProperty("os.version") + "\n" +
               "Locale: " + Locale.getDefault().toString() + "\n";
         infoTextArea.setText(info);
      }
      catch(Exception e)
      {
         e.printStackTrace();
      }
      //imageControl1.setIcon(imageIcon);
      pack();
   }

   private void jbInit() throws Exception
   {
      //imageIcon = new ImageIcon(getClass().getResource("your image name goes here"));
      this.setTitle("About");
      this.setBackground(new Color(255,207,0));
      setResizable(false);
      productTitle.setText(product);
      copyrightLabel1.setText("Copyright (c)2000-2002 Amzi! inc.");
      copyrightLabel2.setText("All Rights Reserved");
      okButton.setText("OK");
      okButton.addActionListener(this);
      urlLabel.setText("www.amzi.com");

      textPanel.setLayout(columnLayout);
      textPanel.setBackground(new Color(255,207,0));
      textPanel.setForeground(new Color(181,48,8));
      textPanel.add(productTitle, null);
      textPanel.add(copyrightLabel1, null);
      textPanel.add(copyrightLabel2, null);
      textPanel.add(urlLabel, null);
      infoScrollPane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      infoScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      infoScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      textPanel.add(infoScrollPane, null);

      buttonPanel.setBackground(new Color(255,207,0));
      buttonPanel.add(okButton, null);

      mainPanel.setLayout(borderLayout);
      mainPanel.setBackground(new Color(255,207,0));
      mainPanel.add(buttonPanel, BorderLayout.SOUTH);
      mainPanel.add(textPanel, BorderLayout.CENTER);
      this.getContentPane().add(mainPanel, null);
   }

   protected void processWindowEvent(WindowEvent e)
   {
      if(e.getID() == WindowEvent.WINDOW_CLOSING)
      {
         cancel();
      }
      super.processWindowEvent(e);
   }

   void cancel()
   {
      dispose();
   }

   public void actionPerformed(ActionEvent e)
   {
      if(e.getSource() == okButton)
      {
         cancel();
      }
   }
}