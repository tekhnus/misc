package attention.graphics;


import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;

public class BasicSign implements SignView {
	
	public static final Ellipse2D.Float SHAPE_CIRCLE =
		new Ellipse2D.Float(-50f, -50f, 100f, 100f);
	public static final Path2D.Float SHAPE_TRIANGLE =
		new java.awt.geom.Path2D.Float();
	public static final Rectangle2D.Float SHAPE_RECTANGLE =
		new Rectangle2D.Float(-50f, -100f, 100f, 200f);
	
	static {
		SHAPE_TRIANGLE.moveTo(0f, -50f);
		SHAPE_TRIANGLE.lineTo(-50f, 86.6f - 50f);
		SHAPE_TRIANGLE.lineTo(50f, 86.6f - 50f);
		SHAPE_TRIANGLE.closePath();
	}
	
	private Shape shape;
	private Color background;
	private Color border;
	private Stroke borderStroke;
	
	public BasicSign(Shape shape,
						Color background, Color border,
						float borderWidth) {
		this.shape = shape;
		this.background = background;
		this.border = border;
		this.borderStroke =
				new BasicStroke(borderWidth,
						BasicStroke.CAP_ROUND,
						BasicStroke.JOIN_ROUND);
	}
	
	public void drawWith(Graphics2D g) {
		g.setPaint(background);
		g.fill(shape);
		g.setPaint(border);
		g.setStroke(borderStroke);
		g.draw(shape);
	}

	@Override
	public Shape getShape() {
		return shape;
	}
}
