package geom;

import java.util.Random;

public class Vector{
    public double x,y,z;
    public static Random r=new Random();;
    public Vector(double vX,double vY,double vZ){
        x=vX;
        y=vY;
        z=vZ;
    }
    public Vector(Point start,Point end){
        x=end.x-start.x;
        y=end.y-start.y;
        z=end.z-start.z;
    }
    public static Vector mul(Vector v,double k){
        return new Vector(v.x*k,v.y*k,v.z*k);
    }
    public static double abs(Vector v){
        return Math.sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
    }
    public static double abs2(Vector v){
        return v.x*v.x+v.y*v.y+v.z*v.z;
    }
    public static Vector norm(Vector v,double len){
        return mul(v,len/abs(v));
    }
    public static Vector norm(Vector v){
        return norm(v,1);
    }
    public static double mul(Vector v1,Vector v2){
        return v1.x*v2.x+v1.y*v2.y+v1.z*v2.z;
    }
    public static double cos(Vector v1,Vector v2){
        return mul(v1,v2)/(abs(v1)*abs(v2));
    }
    public static Vector norm(Vector v1,Vector v2){
        return new Vector(v1.y*v2.z-v1.z*v2.y,v1.z*v2.x-v1.x*v2.z,v1.x*v2.y-v1.y*v2.x);
    }
    public static Vector proj(Vector v,Vector axis){
        return norm(axis,mul(v,axis)/abs(axis));
    }
    public static Vector sub(Vector v1,Vector v2){
        return new Vector(v1.x-v2.x,v1.y-v2.y,v1.z-v2.z);
    }
    public static Vector add(Vector v1,Vector v2){
        return new Vector(v1.x+v2.x,v1.y+v2.y,v1.z+v2.z);
    }
    public static Vector refl(Vector v,Vector mirr){
        Vector t=norm(mirr,mul(v,mirr)/abs(mirr));
        return sub(mul(t,2),v);
    }
    public static Vector neg(Vector v){
        return new Vector(-v.x,-v.y,-v.z);
    }
    public static Vector refr(Vector v,Vector norm,double ior){
        double cosa=Vector.cos(neg(v),norm);
        if(cosa<0)return refr(v,neg(norm),1/ior);
        double sin2a=1-cosa*cosa;
        double sina=Math.sqrt(sin2a);
        double tga=sina/cosa;
        double sinb=sina/ior;
        double cosb=Math.sqrt(1-sinb*sinb);
        double tgb=sinb/cosb;
        Vector proj=proj(v,norm);
        return Vector.add(v,mul(proj,tga/tgb-1));
    }
    public static Vector rand(){
        return new Vector(r.nextDouble(),r.nextDouble(),r.nextDouble());
    }
    public static Vector rand(Vector v){
        return new Vector(v.x*(r.nextDouble()-0.5)/10,v.y*(r.nextDouble()-0.5)/10,v.z*(r.nextDouble()-0.5)/10);
    }
    /*public static Vector refr(Vector v,Vector norm,double ior){
        double cosAlpha=-Vector.cos(v,norm);
        boolean inner=cosAlpha<0;
        double sin2Alpha=1-cosAlpha*cosAlpha;
        double sin2Beta;
        if(inner)return v;
        if(!inner)sin2Beta=sin2Alpha/(ior*ior);
        else sin2Beta=sin2Alpha*ior*ior;
        double tgBeta=Math.sqrt(1/(1-sin2Beta)-1);
        Vector proj=Vector.norm(norm,Vector.mul(v,norm)/Vector.abs(norm));
        Vector ret=Vector.add(proj,Vector.norm(Vector.sub(v,proj),tgBeta*Vector.abs(proj)));
        return ret;
    }*/
}
