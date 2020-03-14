package figures;
import geom.Point;
import geom.Vector;
import mainpack.Const;
import rays.Ray;
import rays.TracingData;
public class Plane extends ShapeData{
    public Vector norm;
    public Point root;
    public Plane(Vector pNorm,Point pRoot){
        norm=pNorm;
        root=pRoot;
    }
    Vector norm(Point p){
        return Vector.norm(norm);
    }
    public TracingData trace(Ray r){
        Vector p=new Vector(r.start,root);
        double k=Vector.mul(p,norm)/Vector.mul(r.dir,norm);
        if(k<=Const.eps)return null;
        Point cls=Point.add(r.start,Vector.mul(r.dir,k));
        return new TracingData(cls,null);
    }
}
