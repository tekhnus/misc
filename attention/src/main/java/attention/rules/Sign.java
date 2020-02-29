package attention.rules;

public class Sign {
	
	public static final Sign SPEED_LIMIT_40 =
			new Sign(Permissions.SPEED_LIMIT_40,
					EffectArea.UNTIL_CROSSING);
	
	private Permissions permissions;
	private EffectArea area;
	
	private Sign(Permissions permissions, EffectArea area) {
		this.permissions = permissions;
		this.area = area;
	}
	
	public Permissions getPermissions() {
		return permissions;
	}
	
	public EffectArea getEffectArea() {
		return area;
	}
}
