package figures;
import geom.*;
import rays.*;
public class Figure{
    public ShapeData shape;
    public ColorData color;
    public Shader shader;
    public Figure(ShapeData fShape,ColorData fColor,Shader fShader){
        shape=fShape;
        color=fColor;
        shader=fShader;
    }
    public Vector norm(Point p){
        return shape.norm(p);
    }
    public TracingData trace(Ray r){
        TracingData tmp=shape.trace(r);
        if(tmp==null)return null;
        return new TracingData(shape.trace(r),this);
    }
    public Color col(Point p){
        return color.col(p);
    }
}
