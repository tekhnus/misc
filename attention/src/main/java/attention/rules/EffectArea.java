package attention.rules;

public class EffectArea {
	
	public static EffectArea UNTIL_CROSSING =
		new EffectArea().whereExpiresWithCrossing(true);
	
	private boolean expiresWithCrossing;
	private boolean expiresAfterDistance;
	private float expirationDistance;
		
	public EffectArea() {
		this.expiresWithCrossing = false;
		this.expiresAfterDistance = false;
		this.expirationDistance = 0f;
	}
	
	public boolean expiresWithCrossing() {
		return expiresWithCrossing;
	}
	
	public boolean expiresAfter(float distance) {
		return expiresAfterDistance &&
					(distance > expirationDistance);
	}
	
	public EffectArea whereExpiresWithCrossing(
			boolean expiresWithCrossing) {
		this.expiresWithCrossing = expiresWithCrossing;
		return this;
	}
	
	public EffectArea whichExpiresAfterDistance(
			float expirationDistance) {
		this.expiresAfterDistance = true;
		this.expirationDistance = expirationDistance;
		return this;
	}
}
