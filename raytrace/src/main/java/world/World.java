package world;
import geom.Poly;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import rays.Camera;
import rays.Poly2D;
public class World{
	private static World inst=new World();
	private Poly polygons[];
	private Poly2D polygons2D[];
	private int sz;
	private World(){
		polygons=new Poly[100000];
		polygons2D=new Poly2D[100000];
		sz=0;
	}
	public static World getWorld(){
		return inst;
	}
	public void addPoly(Poly p){
		polygons[sz]=p;
		polygons2D[sz]=new Poly2D(p);
		sz++;
	}
	public void recalc(){
		for(int i=0;i<sz;i++){
			polygons2D[i].reshut();
			polygons2D[i].updateColor();
		}
	}
	public int getSize(){
		return sz;
	}
	public Poly2D[] getPolygons(){
		return polygons2D;
	}
}
