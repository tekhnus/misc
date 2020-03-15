package edem_world;
import edem_view.ImageFactory;
import java.awt.Image;
import java.util.HashMap;
import java.util.Map;
public class BuildingFactory{
    private final static String names[]={"tent","fire","gradka","field","zagon"};
    private final static int count=names.length;
    private static Image images[];
    private static Map<String,Image>map;
    static{
        images=new Image[count];
        map=new HashMap<String,Image>();
        try{
            for(int i=0;i<count;i++){
                images[i]=ImageFactory.fromFile(names[i]+".bmp");
                map.put(names[i],images[i]);
            }
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }
    public static Building generate(String name,int x,int y){
        if(map.containsKey(name)){
            Building tmp=new Building(name,map.get(name),x,y);
            return tmp;
        }
        else{
            return null;
        }
    }
}
