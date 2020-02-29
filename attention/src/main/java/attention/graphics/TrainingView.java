package attention.graphics;

import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;

import attention.training.SignWithPosition;
import attention.training.Training;
import attention.training.TrainingDef;

public class TrainingView implements Drawable {
	private Training training;
	private TrainingDef def;
	private Landscape landscape;
	
	public TrainingView(Training training) {
		this.training = training;
		this.def = training.getDef();
		this.landscape = new Landscape();
	}

	@Override
	public void drawWith(Graphics2D g) {
		landscape.drawWith(g);
		for (float dist : def.getCrossings()) {
			g.setTransform(new AffineTransform());
			landscape.drawCrossing(g, dist - training.getX());
		}
		for (SignWithPosition signPos : def.getSignPos()) {
			float distance = signPos.getPosition().getX() -
								training.getX();
			g.setTransform(new AffineTransform());
			landscape.drawSign(g, signPos.getSign(), distance);
		}
	}
}
