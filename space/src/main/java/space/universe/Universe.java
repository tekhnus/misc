package space.universe;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Random;
import space.math.MathUtils;
import space.math.Vec2;

public class Universe extends Observable
{
    public static final double gconst = 6.67e-11;
    public int step = 2000;
    private ArrayList<Body> obj;
    private Random r = new Random();
    
    public Universe()
    {
        obj = new ArrayList<Body>();
    }
    
    public void addBody(Body b)
    {
        obj.add(b);
        setChanged();
        notifyObservers();
    }
    
    public void removeBody(Body b)
    {
        obj.remove(b);
        setChanged();
        notifyObservers();
    }
    
    /**
     * Simulate one step
     */
    
    public void simulate()
    {
        for(int i = 0; i < obj.size(); i++)
        {
            Vec2 force = new Vec2(0, 0);
            for(int j = 0; j < obj.size(); j++)
            {
                if(j == i)
                    continue;
                Vec2 conn = MathUtils.diff(obj.get(j).getCoord(), obj.get(i).getCoord());
                double r2 = conn.abs2();
                double m = obj.get(j).getMass();
                if(m < r2)//TODO Really safe?
                {
                    continue;
                }
                force.add(MathUtils.trim(conn, gconst * m / r2));
            }
            obj.get(i).simulate(force, step);
        }
        
        for(int i = 0; i < obj.size(); i++)
        {
            for(int j = 0; j < obj.size(); j++)
            {
                if(i == j)
                    continue;
                if(MathUtils.qDist(obj.get(i).getCoord(), obj.get(j).getCoord()) > obj.get(i).getRadius() + obj.get(j).getRadius())
                {
                    continue;
                }
                if(MathUtils.diff(obj.get(i).getCoord(), obj.get(j).getCoord()).abs() <= obj.get(i).getRadius() + obj.get(j).getRadius())
                {
                    //System.err.println("Collapse!");
                    if(obj.get(i).getMass() > obj.get(j).getMass())
                        obj.remove(j);
                    else
                        obj.remove(i);
                }
            }
        }
        setChanged();
        notifyObservers();
    }
    
    public void dump()
    {
        for(int i = 0; i < obj.size(); i++)
        {
            System.err.printf("Body number %d: name %s; pos %f, %f\n", i, obj.get(i).getName(), obj.get(i).getCoord().get(0), obj.get(i).getCoord().get(1));
        }
        System.err.println("--END DUMP--");
    }
    
    public ArrayList<Body> getBodies()
    {
        return obj;
    }
}
