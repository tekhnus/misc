package rays;
import geom.*;
import geom.Math;
public class Point2D{
	private double x,y;
	private Point base,pr;
	public Point2D(Point p){
		base=p;
	}
	public void reshut(){
		pr=Math.proj(base,Camera.getCamera().getSurf());
		Point o=Camera.getCamera().getSurf().getBase();
		Vector v=new Vector(o,pr);
		Vector bx=Camera.getCamera().getBasisX();
		Vector by=Camera.getCamera().getBasisY();
		double a,b;
		/*if((by.getX()*bx.getY()+bx.getX()*by.getY())!=0)b=(v.getX()*bx.getY()+bx.getX()*v.getY())/(by.getX()*bx.getY()+bx.getX()*by.getY());
		else 											b=(v.getZ()*bx.getY()+bx.getZ()*v.getY())/(by.getZ()*bx.getY()+bx.getZ()*by.getY());
		if(bx.getX()!=0)a=(v.getX()-b*by.getX())/bx.getX();
		else if(bx.getY()!=0)a=(v.getY()-b*by.getY())/bx.getY();
		else a=(v.getZ()-b*by.getZ())/bx.getZ();*/
		a=Math.scalar(v,bx);
		b=Math.scalar(v,by);
		x=a;
		y=b;
	}
	public double getX(){
		return x;
	}
	public double getY(){
		return y;
	}
}
