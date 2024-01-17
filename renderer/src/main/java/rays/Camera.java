package rays;
import geom.*;
import geom.Math;
public class Camera{
	private static Camera inst=new Camera(new Plane(new Point(30,30,30),new Vector(1,1,1)));
	private Plane surf;
	private Vector basisX,basisY;
	public Vector getBasisX(){
		return basisX;
	}
	public Vector getBasisY(){
		return basisY;
	}
	public Plane getSurf(){
		return surf;
	}
	private Camera(Plane surf0){
		surf=surf0;
		recalcBasis();
	}
	public static Camera getCamera(){
		return inst;
	}
	public Poly2D shut(Poly p){
		return new Poly2D(p);
	}
	public void setNewNormal(Vector n){
		surf=new Plane(surf.getBase(),n);
		recalcBasis();
	}
	public void setNewBase(Point p){
		surf=new Plane(p,surf.getNormal());
		recalcBasis();
	}
	private void recalcBasis(){
		basisX=Math.to1(Math.norm(surf.getNormal()));
		basisY=Math.to1(Math.mul(surf.getNormal(),basisX));
	}
}
