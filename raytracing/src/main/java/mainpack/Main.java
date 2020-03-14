package mainpack;
import figures.*;
import geom.Point;
import geom.Vector;
import java.util.Random;
import rays.*;
public class Main{
    public static void main(String[] args){
        final int w=640;
        final int h=480;
        Root rt=new Root(w,h);
        Scene s=new Scene();
        Shader sh=new Shader(s,0.1,0.4,0,0.8,1.5);
        Shader fl=new Shader(s,1,0.4,0,0,0);
        Figure f=new Figure(new Sphere(new Point(3,3,2),2),new SolidColor(new Color(1,0,0)),sh);
        s.addFigure(f);
        Figure g=new Figure(new Sphere(new Point(1,8,1.5),1.5),new SolidColor(new Color(0,1,0)),sh);
        s.addFigure(g);
        Figure p=new Figure(new Sphere(new Point(-1.5,1,1),1),new SolidColor(new Color(0,0,1)),sh);
        s.addFigure(p);
        Figure t=new Figure(new Plane(new Vector(0,0,1),new Point(0,0,0)),new CheckerColor(),fl);
        s.addFigure(t);
        Figure u=new Figure(new Plane(new Vector(0,-1,0),new Point(0,12,0)),new CheckerColor(),sh);
        //s.addFigure(u);
        Figure v=new Figure(new Plane(new Vector(-1,0,0),new Point(12,12,0)),new CheckerColor(),sh);
        //s.addFigure(v);
        Light lg=new Light(new Point(-4,0,2),0.6,15);
        s.addLight(lg);
        Light ps=new Light(new Point(3,3,20),0.2,20);
        s.addLight(ps);
        final Camera cam=new Camera(new Point(0,0,3),6,new Vector(3,4,-3),w,h,0.01);
        cam.render(s,rt);
        
        /*
        Scene s=new Scene();
        Shader sh=new Shader(s,0.6,0.2,0.5,0,0);
        s.addFigure(new Figure(new Sphere(new Point(2.5,0,0.64),0.64),new SolidColor(new Color(1,1,1)),sh));
        s.addFigure(new Figure(new Sphere(new Point(0,0,1),1),new SolidColor(new Color(1,1,1)),sh));
        s.addFigure(new Figure(new Plane(new Vector(0,0,1),new Point(0,0,0)),new SolidColor(new Color(1,1,1)),sh));
        s.addLight(new Light(new Point(7.5,1,3.6),1,30));
        final Camera cam=new Camera(new Point(7.5,-6.5,5.3),300,new Vector(-1,1,-0.7),w,h,0.01);
        cam.render(s,rt);
        */
        /*
        Scene s=new Scene();
        Shader sh=new Shader(s,0.6,0.7,0.5,0,0);
        final Camera cam=new Camera(new Point(0,0,5),10,new Vector(0,1,-0.1),w,h,0.01);
        s.addLight(new Light(new Point(0,20,20),1,25));
        s.addFigure(new Figure(new Plane(new Vector(0,0,1),new Point(0,0,0)),new CheckerColor(),sh));
        Random r=new Random();
        for(int i=0;i<30;i++){
            int x=10-r.nextInt(20);int y=r.nextInt(15)+10;int z=r.nextInt(10)+1;double rad=r.nextInt(15)/10.0+0.5;
            double red=r.nextDouble();double green=r.nextDouble();double blue=r.nextDouble();
            s.addFigure(new Figure(new Sphere(new Point(x,y,z),rad),new SolidColor(new Color(red,green,blue)),sh));
        }
        cam.render(s,rt);
        */
    }
}
