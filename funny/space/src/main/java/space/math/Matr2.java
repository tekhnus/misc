package space.math;

/**
 * Matrix 2x2
 */

public class Matr2
{
    private double[][] data;
    
    public Matr2(Vec2... cols)
    {
        data = new double[2][2];
        for(int i = 0; i < 2; i++)
            data[i] = cols[i].toCol();
    }
    
    public Vec2 getStr(int ind)
    {
        double[] ret = new double[2];
        for(int i = 0; i < 2; i++)
        {
            ret[i] = data[i][ind];
        }
        return new Vec2(ret);
    }
    
    public Vec2 getCol(int ind)
    {
        return new Vec2(data[ind]);
    }
    
    public void set(int str, int col, double newVal)
    {
        data[col][str] = newVal;
    }
    
}
