package figures;
import geom.Point;
import rays.Color;
public class CheckerColor extends ColorData{
    Color col(Point p){
        if((Math.round(p.x)+Math.round(p.y)+Math.round(p.z))%2==0)return new Color(0,0,0);
        return new Color(1,1,1);
    }
}
