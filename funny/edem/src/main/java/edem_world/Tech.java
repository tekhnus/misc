package edem_world;
import edem_files.TechLoader;
import java.util.Observable;
import java.util.Vector;
public class Tech extends Observable{
    public static class Node{
        private String name;
        private Vector<Integer>needs;
        private int number;
        private boolean active;
        private static int lastNumber=0;
        private String building;
        private int ground;
        public Node(String name){
            this.number=lastNumber;
            lastNumber++;
            this.name=name;
            needs=new Vector<Integer>();
        }
        public void setGround(int ground){
            this.ground=ground;
        }
        public int getGround(){
            return ground;
        }
        public void addBuilding(String building){
            this.building=building;
        }
        private Node getNode(int number){
            return nodes.get(number);
        }
        public boolean isActive(){
            return active;
        }
        public void setActive(boolean active){
            this.active=active;
        }
        public boolean canExplore(){
            for(int i=0;i<needs.size();i++){
                if(!getNode(needs.get(i)).isActive())return false;
            }
            return true;
        }
        public String getName(){
            return name;
        }
        public int getNumber(){
            return number;
        }
        public String getBuilding(){
            return building;
        }
        public void addNeeds(int number){
            needs.add(number);
        }
    }
    private static Tech inst=new Tech();
    private static Vector<Node>nodes;
    private Tech(){
        nodes=new Vector<Node>();
    }
    public static Tech getTech(){
        return inst;
    }
    public void addNode(Node node){
        nodes.add(node);
        setChanged();
        notifyObservers(node);
    }
    public void exploreNode(int number){
        if(nodes.get(number).canExplore()){
            nodes.get(number).setActive(true);
            if(nodes.get(number).getBuilding()!=null){
                System.out.println(nodes.get(number).getBuilding());
                World.getWorld().addBuilding(nodes.get(number).getBuilding(),nodes.get(number).getGround());
            }

            setChanged();
            notifyObservers();
        }
        else{
            System.err.println("Tried to explore unavailable technology");
        }

    }
    public Vector<Integer>getExplorable(){
        Vector<Integer>ret=new Vector<Integer>();
        for(int i=0;i<nodes.size();i++){
            if(nodes.get(i).canExplore()&&!nodes.get(i).isActive()){
                ret.add(i);
            }
        }
        return ret;
    }
}
