package geom;
public class Point{
    public double x,y,z;
    public Point(double pX,double pY,double pZ){
        x=pX;
        y=pY;
        z=pZ;
    }
    public static Point add(Point p,Vector v){
        return new Point(p.x+v.x,p.y+v.y,p.z+v.z);
    }
    public static double dist(Point p1,Point p2){
        return Vector.abs(new Vector(p1,p2));
    }
    public static double dist2(Point p1,Point p2){
        return Vector.abs2(new Vector(p1,p2));
    }
}
