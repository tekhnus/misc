package figures;
import geom.*;
import rays.*;
public abstract class ShapeData{
    abstract Vector norm(Point p);
    abstract TracingData trace(Ray r);
}
