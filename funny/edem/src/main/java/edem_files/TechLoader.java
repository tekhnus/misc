package edem_files;
import edem_main.Const;
import edem_world.Tech;
import edem_world.Tech.Node;
import edem_world.World;
import java.io.*;
import java.util.StringTokenizer;
public class TechLoader{
    public static void loadTechTree(String filename){
        try{
            BufferedReader inp=new BufferedReader(new InputStreamReader(TechLoader.class.getResourceAsStream("/" + filename)));
            String s="";
            StringTokenizer st;
            while((s=inp.readLine())!=null){
                st=new StringTokenizer(s," ");
                String name=st.nextToken();
                Node tmp=new Node(name);
                String str;
                boolean first=true;
                int type=0;
                while(st.hasMoreTokens()){
                    str=st.nextToken();
                    if(str.startsWith("-")){
                        tmp.addBuilding(str.substring(1));
                    }
                    else{
                        if(first){
                            type=Integer.parseInt(str);
                            tmp.setGround(type);
                            first=false;
                        }
                        else{
                            tmp.addNeeds(Integer.parseInt(str) - 1);
                        }
                    }
                }
                Tech.getTech().addNode(tmp);
            }
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }
    public static void loadTerrain(String filename){
        try{
            BufferedReader inp=new BufferedReader(new InputStreamReader(TechLoader.class.getResourceAsStream("/" + filename)));
            String s;
            for(int i=0;i<Const.SZ;i++){
                s=inp.readLine();
                for(int c=0;c<Const.SZ;c++){
                    World.getWorld().setType(i,c,s.charAt(c)-'0');
                }
            }
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }
}
