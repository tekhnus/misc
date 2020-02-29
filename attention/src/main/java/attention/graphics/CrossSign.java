package attention.graphics;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;

public class CrossSign implements SignView {
	
	public enum CrossType {
		SLASH, BACKSLASH, BOTH
	}
	
	private SignView underlay;
	private CrossType crossType;
	private Stroke stroke;
	private Color color;

	public CrossSign(SignView underlay, CrossType crossType,
			float strokeWidth, Color color) {
		this.underlay = underlay;
		this.crossType = crossType;
		this.stroke = new BasicStroke(strokeWidth,
				BasicStroke.CAP_SQUARE,
				BasicStroke.JOIN_ROUND);
		this.color = color;
	}

	@Override
	public void drawWith(Graphics2D g) {
		underlay.drawWith(g);
		g.setPaint(color);
		g.setStroke(stroke);
		Rectangle bounds = underlay.getShape().getBounds();
		if (crossType == CrossType.BACKSLASH || crossType == CrossType.BOTH) {
			g.drawLine(bounds.x, bounds.y,
					bounds.x + bounds.width, bounds.y + bounds.height);
		}
		if (crossType == CrossType.SLASH || crossType == CrossType.BOTH) {
			g.drawLine(bounds.x + bounds.width, bounds.y,
					bounds.x, bounds.y + bounds.height);
		}
	}

	@Override																																																																												
	public Shape getShape() {
		return underlay.getShape();
	}
}
