package rays;
public class Color{
    public static final Color BG=new Color(0,0,0);
    public double r,g,b;
    public Color(double cRed,double cGreen,double cBlue){
        r=cRed;
        g=cGreen;
        b=cBlue;
    }
    public static Color mul(Color c,double k){
        return new Color(c.r*k,c.g*k,c.b*k);
    }
    public static Color add(Color c1,Color c2){
        return new Color(c1.r+c2.r,c1.g+c2.g,c1.b+c2.b);
    }
}
