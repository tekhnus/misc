package space.universe;

import java.awt.Color;
import space.graphics.GraphicsBase;
import space.math.MathUtils;
import space.math.Vec2;

/**
 * Simple physical body
 */

public class Body
{
    private Vec2 coord;
    private Vec2 vel;
    private BodyView view;
    private double mass_;
    private String name_;
    private double rad_;
    private boolean focus_;
    private final double minscale_;
    
    /**
     * Create a new body
     * @param name Body name
     * @param init_coord Initial location
     * @param init_vel Initial velocity
     * @param rad Radius
     * @param mass Mass
     * @param col Color
     */
    
    public Body(String name, Vec2 init_coord, Vec2 init_vel, double rad, double mass, Color col, boolean focus, double minscale)
    {
        coord = init_coord;
        vel = init_vel;
        view = new BodyView(col, rad);
        mass_ = mass;
        name_ = name;
        rad_ = rad;
        focus_ = focus;
        minscale_ = minscale;
    }
    
    public Body(String name, Vec2 init_coord, double rad, double mass, Color col, boolean skip, double minscale)
    {
        this(name, init_coord, new Vec2(0, 0), rad, mass, col, skip, minscale);
    }
    
    /**
     * 
     * @param force Force influensing on target
     * @param step Time step of simulation
     */
    
    public void simulate(Vec2 force, int step)
    {
        vel.add(MathUtils.mul(force, step));
        coord.add(MathUtils.mul(vel, step));
    }
    
    public void paint(GraphicsBase g, int x, int y, double size_scale)
    {
        view.paint(g, x, y, size_scale);
    }
    
    public Vec2 getCoord()
    {
        return coord;
    }
    
    public double getMass()
    {
        return mass_;
    }
    
    public String getName()
    {
        return name_;
    }
    
    public double getRadius()
    {
        return rad_;
    }
    
    public boolean getFocus()
    {
        return focus_;
    }
    
    public double getMinscale()
    {
        return minscale_;
    }
}
