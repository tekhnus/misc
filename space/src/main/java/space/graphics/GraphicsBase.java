package space.graphics;

import java.awt.Color;

public abstract class GraphicsBase
{
    public abstract void setColor(Color c);
    public abstract void drawCircle(int x, int y, int rad);
    public abstract void drawString(String s, int x, int y);
}
