package geom;
public class Plane{
	private Point base;
	private Vector normal;
	public Point getBase(){
		return base;
	}
	public Vector getNormal(){
		return normal;
	}
	public Plane(Point base0,Vector normal0){
		base=base0;
		normal=normal0;
	}
	public Plane(Point p1,Point p2,Point p3){
		base=p1;
		Vector v1=new Vector(p2,p3);
		Vector v2=new Vector(p2,p1);
		normal=Math.mul(v1,v2);
	}
}
