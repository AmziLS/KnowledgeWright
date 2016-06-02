
//Title:
//Version:
//Copyright:
//Author:
//Company:
//Description:

package amzi.ide;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URL;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;

public class HTMLFrame extends JInternalFrame implements HyperlinkListener
{
   BorderLayout borderLayout1 = new BorderLayout();
   JScrollPane htmlScroller = new JScrollPane();
   JEditorPane html = new JEditorPane();
   JPanel buttonPanel = new JPanel();
   JButton back = new JButton();
   JButton html_text = new JButton();
//   JList itemList = new JList();
//   JScrollPane listScroller = new JScrollPane();
//   JSplitPane splitter = new JSplitPane();
   App app;

   Vector urlsBack = new Vector();
   URL lastURL = null;
   String origText = null;
   URL origURL;

   public HTMLFrame(App app, String title, String initialDir)
   {
      super(title, true, true, true, true);
      this.app = app;
      try
      {
         jbInit();
         origURL = new URL("file:"+app.tempDirectory+"kwtemp.html");
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
   }

   public void jbInit() throws Exception
   {
      this.setClosable(true);
      this.setIconifiable(true);
      this.setMaximizable(true);
      this.setResizable(true);
      this.getContentPane().setLayout(borderLayout1);

      html.setContentType("text/html");
      html.getEditorKit().createDefaultDocument();
      html.setEditable(false);
      ((HTMLDocument)html.getDocument()).setPreservesUnknownTags(true);
      html.addHyperlinkListener(this);
      htmlScroller.getViewport().add(html, null);
      htmlScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
      this.getContentPane().add(htmlScroller, BorderLayout.CENTER);


//      itemList.setFont(app.getUserFont());
//      listScroller.getViewport().add(itemList, null);
//      listScroller.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);

//      splitter.setOrientation(JSplitPane.VERTICAL_SPLIT);
//      splitter.setBorder(null);
//      splitter.add(listScroller, JSplitPane.TOP);
//      splitter.add(htmlScroller, JSplitPane.BOTTOM);
//      splitter.setDividerLocation(100);
//      this.getContentPane().add(splitter, BorderLayout.CENTER);

//      HTMLEditorKit kit = new HTMLEditorKit();
//      HTMLDocument doc = (HTMLDocument)(kit.createDefaultDocument());
//      html.setEditorKit(kit);
//      html.setDocument(doc);

      back.setText("Back");
      back.setMnemonic(KeyEvent.VK_B);
      back.setEnabled(false);
      back.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            back_actionPerformed(e);
         }
      });
      html_text.setText("HTML <-> Text");
      html_text.setMnemonic(KeyEvent.VK_H);
      html_text.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(ActionEvent e) {
            html_text_actionPerformed(e);
         }
      });
      buttonPanel.add(back, null);
      buttonPanel.add(html_text, null);
      buttonPanel.setLayout(new FlowLayout());
      this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
   }

   public String getText()
   {
      String s = html.getText();
      return s;
   }

   public void setContents(String text/*, Vector list*/)
   {
//      itemList.setListData(list);
      html.setText(text);
      html.setCaretPosition(0);
      origText = null;
      urlsBack.removeAllElements();
      this.show();
   }

   public void hyperlinkUpdate(HyperlinkEvent e)
   {
      if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
      {
         JEditorPane pane = (JEditorPane) e.getSource();
         if (e instanceof HTMLFrameHyperlinkEvent)
         {
            HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent)e;
            HTMLDocument doc = (HTMLDocument)pane.getDocument();
            doc.processHTMLFrameHyperlinkEvent(evt);
         }
         else
         {
            try
            {
               if (origText == null) origText = pane.getText();
               if (lastURL != null) urlsBack.insertElementAt(lastURL, 0);
               lastURL = e.getURL();
               pane.setPage(e.getURL());
               back.setEnabled(true);
               html_text.setEnabled(false);
            }
            catch (IOException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  "I/O ERROR", JOptionPane.ERROR_MESSAGE);
            }
         }
      }
   }

   void back_actionPerformed(ActionEvent e)
   {
      try
      {
         if (urlsBack.size() == 0)
         {
            html.setPage(origURL);
            back.setEnabled(false);
            html_text.setEnabled(true);
         }
         else
         {
            html.setPage((URL)urlsBack.elementAt(0));
            urlsBack.removeElementAt(0);
         }
         lastURL = null;
      }
      catch (IOException ex)
      {
         JOptionPane.showMessageDialog(app, ex.getMessage(),
            "I/O ERROR", JOptionPane.ERROR_MESSAGE);
      }
   }

   void html_text_actionPerformed(ActionEvent e)
   {
      if (html.getContentType().equals("text/html"))
      {
         if (origText == null) origText = html.getText();
         html.setContentType("text/plain");
         html.setText(origText);
      }
      else
      {
         if (origText == null)
            try
            {
               html.setContentType("text/html");
               html.getEditorKit().createDefaultDocument();
               html.setPage(origURL);
            }
            catch (IOException ex)
            {
               JOptionPane.showMessageDialog(app, ex.getMessage(),
                  "I/O ERROR", JOptionPane.ERROR_MESSAGE);
            }
         else
         {
            html.setContentType("text/html");
            html.getEditorKit().createDefaultDocument();
            html.setText(origText);
         }
      }
   }

}


