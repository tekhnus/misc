package mainpack;
import geom.*;
import rays.*;
import figures.*;
import java.util.ArrayList;
public class Scene{
    public ArrayList<Figure> figures;
    public ArrayList<Light> lights;
    public int rays;
    public int photons;
    public ArrayList<Point>amb=new ArrayList<Point>();
    public ArrayList<Double>pwr=new ArrayList<Double>();
    public Scene(){
        figures=new ArrayList<Figure>();
        lights=new ArrayList<Light>();
    }
    public void addFigure(Figure f){
        figures.add(f);
    }
    public void addLight(Light l){
        lights.add(l);
    }
    public TracingData trace(Ray r){
        rays++;
        double min2=Double.MAX_VALUE;
        TracingData bestData=null;
        for(int i=0;i<figures.size();i++){
            TracingData tmp=figures.get(i).trace(r);
            if(tmp!=null){
                double tmpDist2=Point.dist2(r.start,tmp.p);
                if(tmpDist2<min2){
                    bestData=tmp;
                    min2=tmpDist2;
                }
            }
            
        }
        return bestData;
    }
    public boolean inShadow(Point p,Point light){
        TracingData data=trace(new Ray(light,new Vector(light,p),1));
        if(data==null)return false;
        return Vector.abs(new Vector(data.p,p))>Const.eps;
    }
    public Color color(Ray r){
        TracingData data=trace(r);
        if(data==null)return Color.BG;
        return data.f.shader.getColor(r,data);
    }
    public void sendPhoton(Ray r,double pow){
        if(pow<Const.eps)return;
        
        //System.err.println(photons+" photons sent "+pow);
        TracingData data=trace(r);
        if(data==null)return;
        photons++;
        Vector diff=Vector.refl(r.dir,data.f.norm(data.p));
        diff=Vector.mul(diff,-1);
        for(int i=0;i<3;i++){
            Ray nxt=new Ray(data.p,Vector.rand(diff),1);
            sendPhoton(nxt,pow*0.2);
        }
        amb.add(data.p);
        pwr.add(pow);
    }
}
