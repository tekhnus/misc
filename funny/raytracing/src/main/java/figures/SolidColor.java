package figures;
import geom.Point;
import rays.Color;
public class SolidColor extends ColorData{
    public Color color;
    public SolidColor(Color sColor){
        color=sColor;
    }
    Color col(Point p){
        return color;
    }
}
