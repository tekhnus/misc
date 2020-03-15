package rays;
import figures.Plane;
import geom.*;
import mainpack.Root;
import mainpack.Scene;
public class Camera{
    public Point loc;
    public double focal;
    public Vector dir;
    public int w,h;
    public double ratio;
    public Color render[][];
    public Camera(Point cLoc,double cFocal,Vector cDir,int cWidth,int cHeight,double cRatio){
        loc=cLoc;
        focal=cFocal;
        dir=cDir;
        w=cWidth;
        h=cHeight;
        ratio=cRatio;
        render=new Color[w][h];
    }
    public void render(Scene s,Root rt){
        long t1=System.currentTimeMillis();
        s.rays=0;
        int xSmp=2;
        int ySmp=2;
        Vector k=new Vector(0,0,1);
        Vector xAx=Vector.norm(dir,k);
        Vector yAx=Vector.norm(xAx,dir);
        Point eye=Point.add(loc,Vector.norm(dir,-focal));
        for(int x=0;x<w;x++){
            for(int y=0;y<h;y++){
                render[x][y]=new Color(0,0,0);
            }
        }
        double xR=1.0/xSmp;
        double yR=1.0/ySmp;
        double all=xR*yR;
        double hW=(double)w/2;
        double hH=(double)h/2;
        for(int x=0;x<w;x++){
            for(int y=0;y<h;y++){
                double x0=x;
                for(int x1=0;x1<xSmp;x1++){
                    double y0=y;
                    for(int y1=0;y1<ySmp;y1++){
                        Point   cam=Point.add(loc,Vector.norm(xAx,(x0-hW)*ratio));
                        cam=Point.add(cam,Vector.norm(yAx,(y0-hH)*ratio));
                        Vector toCam=new Vector(eye,cam);
                        Ray view=new Ray(eye,toCam,1);
                        render[x][y]=Color.add(render[x][y],s.color(view));
                        y0+=yR;
                    }
                    x0+=xR;
                }
                render[x][y]=Color.mul(render[x][y],all);
                rt.refr(x,y,render[x][y]);
            }
        }
        long t2=System.currentTimeMillis();
        System.err.println("Rendering done in "+(t2-t1)+" millis. "+s.rays/(t2-t1)*1000+" RPS ("+s.rays+" rays)");
    }
}
