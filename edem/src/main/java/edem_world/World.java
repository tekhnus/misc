package edem_world;
import edem_main.Const;
import java.util.Observable;
import java.util.Random;
import java.util.Vector;
public class World extends Observable{
    private int ferrum;
    private int food;
    private int ground[][];//1-desert 2-grass
    private static final int SZ=Const.SZ;
    private Vector<Building>buildings;
    private static World inst=new World();
    private boolean used[][]=new boolean[SZ][SZ];
    private World(){
        buildings=new Vector<Building>();
        ground=new int[SZ][SZ];
        /*for(int i=0;i<SZ;i++){
            for(int c=0;c<SZ;c++){
                ground[i][c]=1;
            }
        }
        ground[4][7]=2;*/
    }
    public static World getWorld(){
        return inst;
    }
    public int getFerrum(){
        return ferrum;
    }
    public int getFood(){
        return food;
    }
    public int getType(int x,int y){
        return ground[x][y];
    }
    public void setType(int x,int y,int type){
        ground[x][y]=type;
        setChanged();
        notifyObservers((x<<12)|y);
    }
    public Vector<Building> getBuildings(){
        return buildings;
    }
    private void addBuilding(String type,int x,int y){
        Building tmp=BuildingFactory.generate(type,x,y);
        if(tmp!=null){
            buildings.add(tmp);
            used[x][y]=true;
            setChanged();
            notifyObservers();
        }
    }
    public void addBuilding(String type,int ground){
        if(hasFreePlace()){
            int[]place=getFreePlace();
            addBuilding(type,place[0],place[1]);
            if(ground!=0){
                setType(place[0],place[1],ground);
            }
        }
    }
    private boolean hasFreePlace(){
        for(int i=1;i<SZ-1;i++){
            for(int c=1;c<SZ-1;c++){
                if(!used[i][c]&&ground[i][c]==1){
                    return true;
                }
            }
        }
        return false;
    }
    private int[] getFreePlace(){
        while(true){
            int x=new Random().nextInt(SZ);
            int y=new Random().nextInt(SZ);
            if(!used[x][y]&&ground[x][y]==1){
                int ret[]={x,y};
                return ret;
            }
        }
    }
}
