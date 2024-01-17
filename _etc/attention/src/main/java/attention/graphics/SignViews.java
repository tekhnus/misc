package attention.graphics;

import java.awt.Color;

import attention.graphics.CrossSign.CrossType;

public class SignViews {
	
	private static SignView grayCross(SignView signView) {
		return new CrossSign(signView, CrossType.SLASH, 30, Color.DARK_GRAY);
	}
	
	public static final SignView WARNING =
			new BasicSign(BasicSign.SHAPE_CIRCLE,
			Color.WHITE, Color.RED, 10f);
	
	public static final SignView CANCELING =
			new BasicSign(BasicSign.SHAPE_CIRCLE,
			Color.WHITE, Color.GRAY, 5f);
	
	public static final SignView DIRECTING =
			new BasicSign(BasicSign.SHAPE_CIRCLE,
			Color.BLUE, Color.WHITE, 5f);
	
	public static final SignView INFO =
			new BasicSign(BasicSign.SHAPE_RECTANGLE,
			Color.WHITE, Color.BLUE, 20f);
	
	public static final SignView PARKING =
			new BasicSign(BasicSign.SHAPE_CIRCLE,
			Color.BLUE, Color.RED, 10f);
	
	public static final SignView speed40 =
			new TextSign(WARNING, "40", Color.BLACK);
		
	public static final SignView speed20 =
			new TextSign(WARNING, "20", Color.BLACK);
		
	public static final SignView unspeed40 =
			grayCross(new TextSign(CANCELING, "40", Color.DARK_GRAY));
		
	public static final SignView unspeed20 =
			grayCross(new TextSign(CANCELING, "20", Color.DARK_GRAY));
		
	public static final SignView unall = grayCross(CANCELING);
		
	public static final SignView straight =
			new TextSign(DIRECTING, "^", Color.WHITE);
		
	public static final SignView bus =
			new TextSign(INFO, "BUS", Color.BLACK);
		
	public static final SignView nopark =
			new CrossSign(PARKING, CrossType.SLASH, 10f, Color.RED);
	
	public static final SignView nostop =
			new CrossSign(PARKING, CrossType.BOTH, 10f, Color.RED);

	public static SignView[] all =
		{speed40, speed20, bus, nopark, nostop};
}
