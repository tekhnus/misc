package figures;
import geom.Point;
import geom.Vector;
import mainpack.Const;
import rays.Ray;
import rays.TracingData;
public class Sphere extends ShapeData{
    public Point center;
    public double radius;
    public Sphere(Point sCenter,double sRadius){
        center=sCenter;
        radius=sRadius;
    }
    Vector norm(Point p){
        return Vector.norm(new Vector(center,p));
    }
    TracingData trace(Ray r){
        Vector p=new Vector(r.start,center);
        double k=Vector.mul(r.dir,p)/Vector.mul(r.dir,r.dir);
        if(k<Const.eps)return null;
        Point near=Point.add(r.start,Vector.mul(r.dir,k));
        double md2=Point.dist2(near,center);
        if(md2>radius*radius)return null;
        double d=Math.sqrt(radius*radius-md2);
        Point cls;
        if(Vector.abs(p)-radius<Const.eps){
            cls=Point.add(near,Vector.norm(r.dir,d));
        }
        else{
            cls=Point.add(near,Vector.norm(r.dir,-d));
        }
        return new TracingData(cls,null);
    }
}
