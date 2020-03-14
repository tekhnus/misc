package rays;
import figures.Plane;
import figures.Sphere;
import geom.Point;
import geom.Vector;
import mainpack.Const;
import mainpack.Scene;
public class Shader{
    public Scene s;
    public double lamb,phong,reflected,refracted,ior;
    public Shader(Scene sScene,double sLambert,double sPhong,double sReflected,double sRefracted,double sIOR){
        s=sScene;
        lamb=sLambert;
        phong=sPhong;
        reflected=sReflected;
        refracted=sRefracted;
        ior=sIOR;
    }
    private Color diffusePhong(Ray view,TracingData data){
        if(data==null){
            return Color.BG;
        }
        double pow=0;
        for(int i=0;i<s.lights.size();i++){
            if(!s.inShadow(data.p,s.lights.get(i).pos)){
                Light light=s.lights.get(i);
                Vector refl=Vector.refl(new Vector(data.p,light.pos),data.f.norm(data.p));
                double cos=-Vector.cos(refl,view.dir);
                double dist2=Point.dist2(light.pos,data.p);
                double lightpow=light.k*dist2+light.pow;
                if(cos<0||lightpow<0){
                    cos=0;
                }
                pow+=lightpow*Math.pow(cos,7);
            }
        }
        pow+=0.1;
        Color diff = Color.mul(data.f.col(data.p),pow*phong);
        return diff;
    }
    private Color diffuseLambert(Ray view,TracingData data){
        if (data == null) {
            return Color.BG;
        }
        double pow = 0;
        for (int i = 0; i < s.lights.size(); i++) {
            if (!s.inShadow(data.p, s.lights.get(i).pos)) {
                Light light=s.lights.get(i);
                double cos = Vector.cos(data.f.norm(data.p), new Vector(data.p,light.pos));
                double dist2=Point.dist2(light.pos,data.p);
                double lightpow=light.k*dist2+light.pow;
                if(cos<0||lightpow<0){
                    cos=0;
                }
                pow += cos * lightpow;
            }
        }
        pow+=0.1;
        Color diff = Color.mul(data.f.col(data.p),pow*lamb);
        return diff;
    }
    private Color reflectionColor(Ray view,TracingData data){
        if(view.energy*reflected<Const.eps){
            return Color.BG;
        }
        if(data==null){
            return Color.BG;
        }
        Vector norm=data.f.norm(data.p);
        if(Vector.cos(view.dir,norm)>0)return Color.BG;
        Vector reflect=Vector.refl(view.dir,norm);
        reflect=Vector.norm(reflect,-1);
        Ray r=new Ray(data.p,reflect,view.energy*reflected);
        return Color.mul(s.color(r),reflected);
    }
    private Color refractionColor(Ray view,TracingData data){
        if(view.energy*refracted<Const.eps){
            return Color.BG;
        }
        if(data==null){
            return Color.BG;
        }

        Vector ref=Vector.refr(view.dir,data.f.norm(data.p),ior);
        Ray r=new Ray(data.p,ref,view.energy*refracted);
        return Color.mul(s.color(r),refracted);
    }
    public Color getColor(Ray view,TracingData data){
        Color lambC=diffuseLambert(view,data);
        Color phongC=diffusePhong(view,data);
        Color reflC=reflectionColor(view,data);
        Color refrC=refractionColor(view,data);
        Color summ=Color.add(Color.add(Color.add(lambC,phongC),reflC),refrC);
        return limitColor(summ);
    }
    private Color limitColor(Color c){
        Color ret=c;
        if(ret.r>1)ret.r=1;
        if(ret.g>1)ret.g=1;
        if(ret.b>1)ret.b=1;
        return ret;
    }
}
