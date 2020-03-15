package space.math;

/**
 * Some math utilites
 */

public class MathUtils
{
    private final static Vec2 nil = new Vec2(0, 0);
    
    public static double qDist(Vec2 v1, Vec2 v2)
    {
        return Math.abs(Math.min(v1.get(0) - v2.get(0), v1.get(1) - v2.get(1)));
    }
    
    public static Vec2 diff(Vec2 v1, Vec2 v2)
    {
        Vec2 ret = new Vec2(0, 0);
        for(int i = 0; i < 2; i++)
            ret.set(i, v1.get(i) - v2.get(i));
        return ret;
    }
    
    public static double dot(Vec2 v1, Vec2 v2)
    {
        double ret = 0;
        for(int i = 0; i < 2; i++)
        {
            ret+= v1.get(i) * v2.get(i);
        }
        return ret;
    }
    
    public static Matr2 mul(Matr2 m1, Matr2 m2)
    {
        Matr2 ret = new Matr2(new Vec2(0, 0), new Vec2(0, 0));
        for(int i = 0; i < 2; i++)
        {
            for(int j = 0; j < 2; j++)
            {
                ret.set(i, j, dot(m1.getStr(i), m2.getCol(j)));
            }
        }
        return ret;
    }
    
    public static Vec2 mul(Matr2 m, Vec2 v)
    {
        Vec2 ret = new Vec2(0, 0);
        for(int i = 0; i < 2; i++)
        {
            ret.set(i, dot(m.getStr(i), v));
        }
        return ret;
    }
    
    public static Vec2 mul(Vec2 v, double k)
    {
        Vec2 ret = new Vec2(0, 0);
        for(int i = 0; i < 2; i++)
            ret.set(i, v.get(i) * k);
        return ret;
    }
    
    public static Vec2 trim(Vec2 v, double newLen)
    {
        return mul(v, newLen / v.abs());
    }
    
    public static Matr2 scale(double k)
    {
        Matr2 ret = new Matr2(new Vec2(k,0), new Vec2(0, k));
        return ret;
    }
    
    public static Matr2 rot(double a)
    {
        double cos = Math.cos(a),
               sin = Math.sin(a);
        Matr2 ret = new Matr2(new Vec2(cos, sin), new Vec2(-sin, cos));
        return ret;
    }
}
