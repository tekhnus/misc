package space.math;

/**
 * 2-dimenional vector
 */

public class Vec2
{
    private double[] coord;
    
    public Vec2(double... col)
    {
        coord = new double[2];
        System.arraycopy(col, 0, coord, 0, 2);
    }
    
    public Vec2(Vec2 toCopy)
    {
        this(toCopy.toCol());
    }
    
    public double[] toCol()
    {
        return coord;
    }
    
    public double get(int ind)
    {
        return coord[ind];
    }
    
    public void set(int ind, double newVal)
    {
        coord[ind] = newVal;
    }
    
    public void add(Vec2 v)
    {
        for(int i = 0; i < 2; i++)
            coord[i] += v.get(i);
    }
    
    public double abs2()
    {
        return MathUtils.dot(this, this);
    }
    
    public double abs()
    {
        return Math.sqrt(abs2());
    }
    
    @Override
    public String toString()
    {
        return coord[0] + ";" + coord[1];
    }

    public void sub(Vec2 v)
    {
        add(MathUtils.diff(new Vec2(0, 0), v));
    }
}
