package attention;


import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

import javax.swing.Timer;

import attention.graphics.MainCanvas;
import attention.graphics.MainFrame;
import attention.graphics.SignView;
import attention.graphics.SignViews;
import attention.graphics.TrainingView;
import attention.training.Position;
import attention.training.Training;
import attention.training.TrainingDef;

public class Main implements Runnable, ActionListener {
	
	private Training training;
	private MainCanvas mainCanvas;

	public static void main(String[] args) {
		new Main();
	}
	
	public Main(){
		TrainingDef trainingDef = new TrainingDef(40f);
		Random rand = new Random();
		int size = SignViews.all.length;
		for(int i = 0; i < 100; ++i) {
			SignView sign = SignViews.all[rand.nextInt(size)];
			Position position = new Position(i * 150f + 100f);
			trainingDef.add(sign, position);
			trainingDef.addCrossing(i * 150f + 175f);
		}
		training = new Training(trainingDef);
		EventQueue.invokeLater(this);
	}

	@Override
	public void run() {
		TrainingView trainingView = new TrainingView(training);
		mainCanvas = new MainCanvas(trainingView);
		MainFrame mainFrame = new MainFrame(mainCanvas);
		mainFrame.setVisible(true);
		Timer timer = new Timer(30, this);
		timer.start();
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		training.step(30);
		mainCanvas.repaint();
	}

}
