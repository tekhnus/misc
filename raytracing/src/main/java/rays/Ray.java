package rays;
import geom.*;
public class Ray{
    public Point start;
    public Vector dir;
    public double energy;
    public Ray(Point rStart,Vector rDir,double rEnergy){
        start=rStart;
        dir=rDir;
        energy=rEnergy;
        //System.out.println(energy);
    }
}
