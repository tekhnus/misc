package happiness;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public class FieldDisplay extends JPanel {
    private final Field<Color> field;
    private final int width, height;
    private final BufferedImage image;

    public FieldDisplay(Field<Color> field, int width, int height) {
        this.field = field;
        this.width = width;
        this.height = height;
        this.image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        this.setBackground(java.awt.Color.BLACK);
    }

    @Override
    public void paint(Graphics graphics) {
        super.paint(graphics);
        Graphics2D graphics2D = (Graphics2D) graphics;
        graphics2D.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        for (int w = 0; w < this.width; ++w) {
            for (int h = 0; h < this.height; ++h) {
                image.setRGB(w, h, field.valueAt(Vec2d.temp(w, h)).toRGBInt());
            }
        }
        int size = Math.max(this.getWidth(), this.getHeight());
        graphics2D.drawImage(image, 0, 0, size, size, null);
    }
}
