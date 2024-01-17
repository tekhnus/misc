package geom;
public class Math{
	public static Point move(Point p,Vector v){
		return new Point(p.getX()+v.getX(),p.getY()+v.getY(),p.getZ()+v.getZ());
	}
	public static Vector mul(Vector v,double n){
		return new Vector(v.getX()*n,v.getY()*n,v.getZ()*n);
	}
	public static double scalar(Vector v1,Vector v2){
		return v1.getX()*v2.getX()+v1.getY()*v2.getY()+v1.getZ()*v2.getZ();
	}
	public static Vector mul(Vector v1,Vector v2){
		return new Vector(v1.getY()*v2.getZ()-v1.getZ()*v2.getY(),v1.getZ()*v2.getX()-v1.getX()*v2.getZ(),v1.getX()*v2.getY()-v1.getY()*v2.getX());
	}
	public static Vector neg(Vector v1){
		return new Vector(-v1.getX(),-v1.getY(),-v1.getZ());
	}
	public static Vector norm(Vector v){
		if(v.getY()==0&&v.getX()==0)return new Vector(1,0,0);
		return new Vector(v.getY(),-v.getX(),0);
	}
	public static double len(Vector v){
		return java.lang.Math.sqrt(scalar(v,v));
	}
	public static Vector to1(Vector v){
		return mul(v,1/len(v));
	}
	public static Vector add(Vector v1,Vector v2){
		return new Vector(v1.getX()+v2.getX(),v1.getY()+v2.getY(),v1.getZ()+v2.getZ());
	}
	public static Vector sub(Vector v1,Vector v2){
		return add(v1,neg(v2));
	}
	public static Point proj(Point p,Plane alpha){
		Vector c=new Vector(p,alpha.getBase());
		double k=scalar(c,alpha.getNormal())/scalar(alpha.getNormal(),alpha.getNormal());
		return move(p,mul(alpha.getNormal(),k));
	}
	public static Vector mirror(Vector v1,Vector v2){
		double k=scalar(v1,v2)/scalar(v2,v2);
		return sub(v2,mul(v1,k));
	}
	public static Poly proj(Poly p,Plane alpha){
		return new Poly(proj(p.getP1(),alpha),proj(p.getP2(),alpha),proj(p.getP3(),alpha),p.getColor());
	}
	public static double dist(Point p,Plane alpha){
		return len(new Vector(p,proj(p,alpha)));
	}
	public static double angle(Vector v1,Vector v2){
		return scalar(v1,v2)/(len(v1)*len(v2));
	}
	public static double angle(Plane p1,Plane p2){
		return angle(p1.getNormal(),p2.getNormal());
	}
}
