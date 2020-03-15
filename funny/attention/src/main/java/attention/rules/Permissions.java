package attention.rules;

public class Permissions {
	
	public static Permissions SPEED_LIMIT_40 =
		new Permissions().withMaxSpeed(40f);
	
	public static Permissions NO_STOP =
		new Permissions().whereCanStop(false);
	
	public static final float NO_SPEED_LIMIT = 1000f;
	
	private float maxSpeed;
	private boolean canStop;
	
	public Permissions() {
		maxSpeed = NO_SPEED_LIMIT;
		canStop = true;
	}
	
	public float maxSpeed() {
		return maxSpeed;
	}
	
	public boolean canStop() {
		return canStop;
	}
	
	public Permissions withMaxSpeed(float maxSpeed) {
		this.maxSpeed = maxSpeed;
		return this;
	}
	
	public Permissions whereCanStop(boolean canStop) {
		this.canStop = canStop;
		return this;
	}
}
