package attention.training;

import attention.rules.EffectArea;
import attention.rules.Permissions;

public class SignWatcher {
	
	private static final float notEncountered = -1f;
	
	private Permissions permissions;
	private EffectArea area;
	
	private float x;
	private boolean crossingEncountered;
	
	public SignWatcher(Permissions permissions,
			EffectArea area) {
		this.permissions = permissions;
		this.area = area;
		
		this.x = notEncountered;
		this.crossingEncountered = false;
	}
	
	public Permissions getPermissions() {
		return permissions;
	}
	
	public void encounter() {
		this.x = 0f;
	}
	
	public void encounterCrossing() {
		if (x != notEncountered)
			crossingEncountered = true;
	}
	
	public void step(float dx) {
		if (this.x != notEncountered)
			this.x += dx;
	}
	
	public boolean isActive() {
		if (x == notEncountered)
			return false;
		return (area.expiresWithCrossing() && 
					crossingEncountered)      ||
			   area.expiresAfter(x);
	}
	
}
