package rays;
import geom.Point;
public class Light{
    public Point pos;
    public double pow;
    public double half;
    public double k;
    public Light(Point lPos,double lPow,double lHalf){
        pos=lPos;
        pow=lPow;
        half=lHalf;
        k=-pow/(2*half*half);
    }
}
