/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2000
 * Company:
 * @author
 * @version 1.0
 */

package amzi.ide;

import javax.swing.*;
import java.awt.*;


public class PersonalLicense extends JWindow //implements Runnable
{
//   JWindow splashScreen;
//   Thread thread;
//   long startTime;

   public PersonalLicense(App app, Dimension deskSize)
   {
      super(app);
      createWindow(deskSize);

//      this.setVisible(true);
//      start();
   }

   private void createWindow(Dimension deskSize)
   {
      JPanel mainPanel = new JPanel();
      JPanel licensePanel = new JPanel();
      BorderLayout borderLayout = new BorderLayout();
      BorderLayout licenseBorderLayout = new BorderLayout();
      JTextArea licenseTextArea = new JTextArea();
      JLabel topLabel = new JLabel();
      JLabel bottomLabel = new JLabel();

      bottomLabel.setForeground(new Color(181, 48, 8));
      bottomLabel.setBackground(new Color(255, 207, 0));
      bottomLabel.setFont(new java.awt.Font("Dialog", 1, 20));
      bottomLabel.setHorizontalAlignment(SwingConstants.CENTER);
      bottomLabel.setText("   Academic/Personal/Evaluation License   ");

      topLabel.setForeground(new Color(181, 48, 8));
      topLabel.setBackground(new Color(255, 207, 0));
      topLabel.setFont(new java.awt.Font("Dialog", 1, 20));
      topLabel.setHorizontalAlignment(SwingConstants.CENTER);
      topLabel.setText("     KnowledgeWright®     ");

      licenseTextArea.setBackground(new Color(255, 207, 0));
      licenseTextArea.setFont(new java.awt.Font("Serif", 1, 14));
      licenseTextArea.setEditable(false);
      licenseTextArea.setMargin(new Insets(10, 10, 10, 10));
      licenseTextArea.setLineWrap(true);
      licenseTextArea.setWrapStyleWord(true);
      licenseTextArea.setText("This free product is licensed for academic, personal or 90-day " +
    "evaluation use (see License Agreement for details). To purchase a " +
    "Professional or Source Code License, e-mail sales@amzi.com.");

      licensePanel.setLayout(licenseBorderLayout);
      licensePanel.setBorder(BorderFactory.createEtchedBorder());
      licensePanel.add(licenseTextArea, BorderLayout.CENTER);

      mainPanel.setLayout(borderLayout);
      mainPanel.setBorder(BorderFactory.createRaisedBevelBorder());
      mainPanel.setBackground(Color.white);
      mainPanel.add(topLabel, BorderLayout.NORTH);
      mainPanel.add(bottomLabel,  BorderLayout.SOUTH);
      mainPanel.add(licensePanel, BorderLayout.CENTER);

      this.getContentPane().add(mainPanel, BorderLayout.CENTER);
      this.pack();
      this.setSize(this.getPreferredSize());
      Dimension plSize = this.getSize();
      this.setLocation((deskSize.width-plSize.width)/2, (deskSize.height-plSize.height)/2);

//      return window;
   }

/*
   public void start()
   {
      startTime = System.currentTimeMillis();
      thread = new Thread(this);
      thread.start();
   }

   public void stop()
   {
      if(thread != null)
      {
         thread.interrupt();
         thread = null;
      }
   }

   public void run()
   {
      splashScreen.validate();
      splashScreen.setVisible(true);
   }

   public void close()
   {
      long now = System.currentTimeMillis();
      if (now - startTime < 5000)
         try { thread.sleep(5000-(now-startTime)); } catch (InterruptedException ex) { }

      stop();
      splashScreen.setVisible(false);
   }
*/
}