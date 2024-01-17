package attention.graphics;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.JPanel;

@SuppressWarnings("serial")
public class MainCanvas extends JPanel {
	
	private TrainingView trainingView;
	
	public MainCanvas(TrainingView traningView) {
		this.setDoubleBuffered(true);
		this.trainingView = traningView;
	}
	
	@Override
	public void paintComponent(Graphics gr) {
		super.paintComponent(gr);
		Graphics2D g = (Graphics2D)gr;
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
						   RenderingHints.VALUE_ANTIALIAS_ON);
		trainingView.drawWith(g);
	}
}
