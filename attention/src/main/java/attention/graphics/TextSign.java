package attention.graphics;


import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

public class TextSign implements SignView {
	
	private static final Font font = new Font(null, Font.BOLD, 40);

	private SignView underlay;
	private String text;
	private Color color;

	public TextSign(SignView underlay, String text, Color color) {
		this.underlay = underlay;
		this.text = text;
		this.color = color;
	}

	@Override
	public void drawWith(Graphics2D g) {
		Rectangle2D textBounds;
		Rectangle2D underlayBounds = underlay.getShape().getBounds2D();
		float centerX = (float)underlayBounds.getCenterX(),
			  centerY = (float)underlayBounds.getCenterY();
		
		underlay.drawWith(g);
		
		g.setPaint(color);
		g.setFont(font);
		
		textBounds = font.getStringBounds(text,
				g.getFontRenderContext());
		float shiftX = centerX - (float)textBounds.getWidth() / 2f,
			  shiftY = centerY + (float)textBounds.getHeight() / 2f;
		
		g.drawString(text, shiftX, shiftY);
	}

	@Override																																																																												
	public Shape getShape() {
		return underlay.getShape();
	}
}
