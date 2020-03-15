package rays;
import java.awt.Color;
import geom.Poly;
public class Poly2D implements Comparable{
	private Point2D p1,p2,p3;
	private double z;
	private Color basicColor;
	private Color color;
	private Poly base;
	public Point2D getP1(){
		return p1;
	}
	public Point2D getP2(){
		return p2;
	}
	public Point2D getP3(){
		return p3;
	}
	public Poly2D(Poly p){
		base=p;
		p1=new Point2D(base.getP1());
		p2=new Point2D(base.getP2());
		p3=new Point2D(base.getP3());
		basicColor=base.getColor();
	}
	public double getZ(){
		return z;
	}
	public void updateColor(){
		double alpha=geom.Math.angle(base.getPlane(),Camera.getCamera().getSurf());
		alpha=Math.abs(alpha);
		double how=2*(alpha/(Math.PI));
		color=new Color((int)(basicColor.getRed()*how),(int)(basicColor.getGreen()*how),(int)(basicColor.getBlue()*how));
	}
	public Color getColor(){
		return color;
	}
	public void reshut(){
		p1.reshut();
		p2.reshut();
		p3.reshut();
		z=base.getZDepth();
	}
	@Override
	public int compareTo(Object o){
		return (int)Math.signum(getZ()-((Poly2D)o).getZ());
	}
}
