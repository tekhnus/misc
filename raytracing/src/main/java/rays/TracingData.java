package rays;
import figures.Figure;
import geom.Point;
public class TracingData{
    /*public Point p;
    public double dist;
    public Figure f;
    public TracingData(Point point,double distance,Figure figure){
        p=point;
        dist=distance;
        f=figure;
    }
    public TracingData(TracingData par,Figure figure){
        if(par!=null){
            p=par.p;
            dist=par.dist;
            f=figure;
        }
    }*/
    public Point p;
    public Figure f;
    public TracingData(Point point,Figure figure){
        p=point;

        f=figure;
    }
    public TracingData(TracingData par,Figure figure){
        if(par!=null){
            p=par.p;
            
            f=figure;
        }
    }
}
