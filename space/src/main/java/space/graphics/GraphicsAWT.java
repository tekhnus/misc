package space.graphics;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

public class GraphicsAWT extends GraphicsBase
{
    private Graphics2D gr;
    
    public GraphicsAWT(Graphics g)
    {
        gr = (Graphics2D)g;
        gr.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    }
    
    @Override
    public void setColor(Color c)
    {
        gr.setColor(c);
    }
    
    @Override
    public void drawCircle(int x, int y, int rad)
    {
        gr.fillOval(x - rad, y - rad, 2 * rad, 2 * rad);
    }

    @Override
    public void drawString(String s, int x, int y)
    {
        gr.drawString(s, x, y);
    }
}
