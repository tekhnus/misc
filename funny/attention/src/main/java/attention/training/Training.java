package attention.training;

public class Training {
	private TrainingDef def;
	private float x;
	
	public Training(TrainingDef def) {
		this.def = def;
		this.x = 0f;
	}
	
	public TrainingDef getDef() {
		return def;
	}
	
	public float getX() {
		return x;
	}
	
	public void step(long ms) {
		x += def.getSpeed() * ms / 1000f;
	}
}
