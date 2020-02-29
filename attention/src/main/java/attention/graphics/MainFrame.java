package attention.graphics;

import javax.swing.JFrame;

@SuppressWarnings("serial")
public class MainFrame extends JFrame {
	
	public MainFrame(MainCanvas mainCanvas) {
		super("Attention!");
		setSize(1000, 600);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		add(mainCanvas);
	}
}
