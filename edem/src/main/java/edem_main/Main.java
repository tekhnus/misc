package edem_main;
import edem_files.TechLoader;
import edem_view.WorldWindow;
import edem_world.World;
public class Main{
    private static Main inst=new Main();
    private static WorldWindow main_fr;
    private static World world;
    private Main(){
        TechLoader.loadTerrain("terrain.data");
        main_fr=new WorldWindow();
        TechLoader.loadTechTree("technology.data");
    }
    public static void main(String[] args){}
}
