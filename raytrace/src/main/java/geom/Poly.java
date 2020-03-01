package geom;
import java.awt.Color;
import rays.Camera;
public class Poly{
	private Point p1,p2,p3;
	private Plane plane;
	private Color color;
	public Point getP1(){
		return p1;
	}
	public Point getP2(){
		return p2;
	}
	public Point getP3(){
		return p3;
	}
	public Poly(Point p10,Point p20,Point p30,Color c){
		p1=p10;
		p2=p20;
		p3=p30;
		color=c;
		plane=new Plane(p1,p2,p3);
	}
	public Point getMedian(){
		return new Point((p1.getX()+p2.getX()+p3.getX())/3.0,(p1.getY()+p2.getY()+p3.getY())/3.0,(p1.getZ()+p2.getZ()+p3.getZ())/3.0);
	}
	public double getZDepth(){
		return Math.dist(getMedian(),Camera.getCamera().getSurf());
	}
	public Color getColor(){
		return color;
	}
	public Plane getPlane(){
		return plane;
	}
}
