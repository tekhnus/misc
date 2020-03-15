/*
Do whatever you want with this code
 */

package harius.snowflakes;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import javax.swing.JPanel;
import mikera.vectorz.Vector2;

public class View extends JPanel
{
    private World world;
    private double scale;
    
    private static final Color SKY = new Color(0, 51, 102).darker().darker();
    private static final Color FLAKE = new Color(255, 253, 223);
    private static final int SIZE = 5;
    
    public View(World world, double scale) {
        this.world = world;
        this.scale = scale;
    }
    
    @Override
    public void paint(Graphics gr) {
        super.paint(gr);
        Graphics2D g = (Graphics2D)gr;
        g.setColor(View.SKY);
        g.fillRect(0, 0, this.getWidth(), this.getHeight());
        g.setColor(View.FLAKE);
        for(Flake flake : this.world.getFlakes()) {
            Vector2 location = flake.getLocation();
            int x = (int)(scale * location.x);
            int y = (int)(scale * (world.getHeight() - location.y));
            int size = (int)(View.SIZE * 1000 * flake.getMass());
            g.fillRect(x, y, size, size);
        }
    }
}
