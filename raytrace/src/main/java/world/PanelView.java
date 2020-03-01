package world;
import java.util.Arrays;
import java.util.List;
import java.awt.*;
import javax.swing.*;
import rays.Poly2D;
public class PanelView extends JPanel{
	private double scale=200;
	private int dx=400,dy=300;
	private Poly2D[] draw;
	private int sz;
	private static int frame=0;
	public PanelView(){
		super();
		setSize(800,600);
		sz=0;
	}
	public void updateAll(){
		updateDraw();
		try{
			Thread.sleep(1000/12);
		}catch(InterruptedException e){
			e.printStackTrace();
		}
		repaint();
	}
	public void updateDraw(){
		draw=World.getWorld().getPolygons();
		sz=World.getWorld().getSize();
		Arrays.sort(draw,0,sz);
		/*for(int i=1;i<sz;i++){
			for(int c=sz-1;c>=i;c--){
				
				if(draw[c].getZ()>draw[c-1].getZ()){
					Poly2D tmp=draw[c];
					draw[c]=draw[c-1];
					draw[c-1]=tmp;
				}
			}
		}*/
	}
	public void paint(Graphics gr){
		Graphics2D g=(Graphics2D)gr;
		//g.setRenderingHint(RenderingHints.KEY_RENDERING,RenderingHints.VALUE_RENDER_QUALITY);
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
		super.paint(g);
		g.setColor(Color.GRAY);
		g.fillRect(0,0,1000,1000);
		for(int i=0;i<sz;i++){
			int xs[]={(int)(draw[i].getP1().getX()*scale)+dx,(int)(draw[i].getP2().getX()*scale)+dx,(int)(draw[i].getP3().getX()*scale)+dx};
			int ys[]={(int)(draw[i].getP1().getY()*scale)+dy,(int)(draw[i].getP2().getY()*scale)+dy,(int)(draw[i].getP3().getY()*scale)+dy};
			//for(int c=0;c<3;c++)System.out.println(xs[c]+" "+ys[c]);
			g.setColor(draw[i].getColor());
			g.fillPolygon(xs,ys,3);
			g.setColor(Color.BLACK);
			//g.drawPolygon(xs,ys,3);
		}
		g.drawString(frame+++" frames",30,30);
	}
}