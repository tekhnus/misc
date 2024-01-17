package geom;
public class Vector{
	private double x,y,z;
	public double getX(){
		return x;
	}
	public double getY(){
		return y;
	}
	public double getZ(){
		return z;
	}
	public Vector(double x0,double y0,double z0){
		x=x0;
		y=y0;
		z=z0;
	}
	public Vector(Point p1,Point p2){
		x=p2.getX()-p1.getX();
		y=p2.getY()-p1.getY();
		z=p2.getZ()-p1.getZ();
	}
}