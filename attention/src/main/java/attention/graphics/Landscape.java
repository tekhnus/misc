package attention.graphics;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.geom.Path2D;

public class Landscape implements Drawable {

	private static final Paint asphalt = Color.GRAY;
	private static final Paint grass = Color.GREEN.darker();
	private static final Paint sky = new Color(100, 100, 200);
	private static final Paint metal = Color.DARK_GRAY;
	private static final float upWidth = 0.1f;
	private static final float downWidth = 2f;
	private static final float skyHeight = 0.5f;
	
	private int widthPx, upWidthPx, downWidthPx;
	private int heightPx, skyHeightPx;
	
	private void updateDimensions(Graphics2D g) {
		widthPx = g.getClipBounds().width;
		upWidthPx = (int)(widthPx * upWidth);
		downWidthPx = (int)(widthPx * downWidth);
		heightPx = g.getClipBounds().height;
		skyHeightPx = (int)(heightPx * skyHeight);
	}
	
	@Override
	public void drawWith(Graphics2D g) {
		updateDimensions(g);
		
		Path2D roadShape = new Path2D.Float();
		roadShape.moveTo((widthPx - downWidthPx) / 2, heightPx);
		roadShape.lineTo((widthPx + downWidthPx) / 2, heightPx);
		roadShape.lineTo((widthPx + upWidthPx) / 2, skyHeightPx);
		roadShape.lineTo((widthPx - upWidthPx) / 2, skyHeightPx);
		roadShape.closePath();
		
		g.setPaint(grass);
		g.fillRect(0, skyHeightPx, widthPx, heightPx);
		
		g.setPaint(sky);
		g.fillRect(0, 0, widthPx, skyHeightPx);
		
		g.setPaint(asphalt);
		g.fill(roadShape);
	}
	
	public void drawSign(Graphics2D g, SignView sign, float distance) {
		if (distance < 0 || distance > 300)
			return;
		
		float visibleDistance = 5f / distance;
		g.translate((
						widthPx +
						upWidthPx * (1 - visibleDistance) +
						downWidthPx * visibleDistance
					) / 2,
					skyHeightPx +
					(heightPx - skyHeightPx) * visibleDistance);
		g.scale(2 * visibleDistance, 2 * visibleDistance);
		
		g.setPaint(metal);
		g.fillRect(-5, -300, 10, 300);
		
		g.translate(0, -300);
		sign.drawWith(g);
	}
	
	public void drawCrossing(Graphics2D g, float distance) {
		if (distance < 0 || distance > 300)
			return;
		
		float visibleDistance = 5f / distance;
		g.translate(0,
			skyHeightPx +
			(heightPx - skyHeightPx) * visibleDistance);
		g.scale(1f, 2 * visibleDistance);
		
		g.setPaint(asphalt);
		g.fillRect(0, -30, widthPx, 60);
	}
}
