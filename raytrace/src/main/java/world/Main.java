package world;
import geom.*;
import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.Math;
import java.util.Random;
import java.util.StringTokenizer;

import javax.swing.JFrame;
import rays.Camera;
public class Main{
	public void fromFile(String name) throws Exception{
		BufferedReader inp=new BufferedReader(new FileReader(name));
		while(inp.ready()){
			Point[]pts=new Point[3];
			for(int i=0;i<3;i++){
				double x=Double.parseDouble(inp.readLine());
				double y=Double.parseDouble(inp.readLine());
				double z=Double.parseDouble(inp.readLine());
				pts[i]=new Point(x,y,z);
			}
			int color[]=new int[3];
			for(int i=0;i<3;i++){
				color[i]=Integer.parseInt(inp.readLine());
			}
			Random r=new Random();
			World.getWorld().addPoly(new Poly(pts[0],pts[1],pts[2],new Color(color[0],color[1],color[2])));
			//World.getWorld().addPoly(new Poly(pts[0],pts[1],pts[2],new Color(0,r.nextInt(256),r.nextInt(256))));
		}
		
	}
	public Main(){
		JFrame mf=new JFrame("render");
		mf.setSize(900,700);
		mf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		PanelView view=new PanelView();
		mf.add(view);
		mf.setVisible(true);
		//
		try{
			fromFile("cube.txt");
		}catch(Exception e){
			e.printStackTrace();
		}
		//
		for(double i=0;;i+=0.1){
			Camera.getCamera().setNewNormal(new Vector(Math.cos(i),Math.sin(i),Math.sin(i/2)));
			Camera.getCamera().setNewBase(new Point(5000*Math.cos(i),5000*Math.sin(i),5000*Math.sin(i/2)));
			World.getWorld().recalc();
			view.updateAll();
			
		}
	}
	public static void main(String[] args){
		new Main();
	}
}