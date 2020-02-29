package attention.training;


import java.util.ArrayList;
import java.util.List;

import attention.graphics.SignView;

public class TrainingDef {

	private List<SignWithPosition> signPos;
	private List<Float> crossings;
	private float speed;
	
	public TrainingDef(List<SignWithPosition> signPos,
			List<Float> crossings,
			float speed) {
		this.signPos = signPos;
		this.crossings = crossings;
		this.speed = speed;
	}
	
	public TrainingDef(float speed) {
		this(new ArrayList<SignWithPosition>(),
				new ArrayList<Float>(), speed);
	}
	
	public void add(SignView sign, Position position) {
		signPos.add(new SignWithPosition(sign, position));
	}
	
	public void addCrossing(float distance) {
		crossings.add(distance);
	}
	
	public List<SignWithPosition> getSignPos() {
		return signPos;
	}
	
	public float getSpeed() {
		return speed;
	}

	public List<Float> getCrossings() {
		return crossings;
	}
}
