package space.universe;

import java.awt.Color;
import space.graphics.GraphicsBase;

/**
 * Contains painting methods
 */

public class BodyView
{
    private Color col_;
    private double rad_;
    private static double lin_cnst = 14000000 * UniverseView.unit_actual * UniverseView.sun_default / UniverseView.sun_actual / UniverseView.unit_default;
    
    public BodyView(Color col, double rad)
    {
        col_ = col;
        rad_ = rad;
    }
    
    public int getRealRadius(double scale)
    {
        int ret = (int)(scale * Math.pow(UniverseView.scale_default / scale, 0.9) * lin_cnst * Math.pow(rad_, 0.2));
        if(ret < 1)
            ret = 1;
        return ret;
    }
    
    public void paint(GraphicsBase g, int x, int y, double scale)
    {
        g.setColor(col_);
        int realRad = getRealRadius(scale);
        g.drawCircle(x, y, realRad);
    }
}
