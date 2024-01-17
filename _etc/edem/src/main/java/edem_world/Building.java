package edem_world;
import java.awt.Graphics2D;
import java.awt.Image;
public class Building{
    private String name;
    private Image image;
    private int x;
    private int y;
    public Building(String name,Image image,int x,int y){
        this.name=name;
        this.image=image;
        this.x=x;
        this.y=y;
    }
    public int getX(){
        return x;
    }
    public int getY(){
        return y;
    }
    public Image getImage(){
        return image;
    }
}
