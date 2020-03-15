package edem_view;
import edem_main.Const;
import edem_world.Building;
import edem_world.World;
import java.util.Observable;
import java.util.Observer;
import javax.swing.*;
import java.awt.*;
import java.util.Vector;
public class WorldPane extends JPanel implements Observer{
    private final static int CELL_SZ=Const.CELL_SZ;
    private final static double rt=Const.RT;
    private final static int SZ=Const.SZ;
    private World world;
    private Image ground[][];
    private Image ground2[][];
    public WorldPane(World world){
        this.world=world;
        this.world.addObserver(this);
        setSize(Const.SIZE_W-Const.SIZE_PANE,Const.SIZE_H);
        ground=new Image[SZ][SZ];
        ground2=new Image[SZ][SZ];
        for(int i=0;i<SZ;i++){
            for(int c=0;c<SZ;c++){
                refrGround(i,c);
            }
        }
    }
    private void refrGround(int i,int c){
        if(world.getType(i,c)==1){
                    ground[i][c]=ImageFactory.generateRomb(CELL_SZ,new Color(0xFFCC33),0.5);
                    ground2[i][c]=ImageFactory.generateSecondary(CELL_SZ,new Color(0xFFCC33),0.7);
                }
                else if(world.getType(i,c)==2){
                    ground[i][c]=ImageFactory.generateRomb(CELL_SZ,Color.BLUE,0.5);
                    ground2[i][c]=ImageFactory.generateSecondary(CELL_SZ,Color.BLUE,0.7);
                }
                else if(world.getType(i,c)==3){
                    ground[i][c]=ImageFactory.generateRomb(CELL_SZ,new Color(153,51,51),0.5);
                    ground2[i][c]=ImageFactory.generateSecondary(CELL_SZ,new Color(153,51,51),0.7);
                }
                else if(world.getType(i,c)==4){
                    ground[i][c]=ImageFactory.generateRomb(CELL_SZ,Color.GREEN,0.5);
                    ground2[i][c]=ImageFactory.generateSecondary(CELL_SZ,Color.GREEN,0.7);
                }
    }
    public void update(Observable o,Object arg){
        if(arg!=null){
            int pos=(Integer)arg;
            refrGround(pos>>12,pos&((1<<12)-1));
        }
        repaint();
    }
    @Override
    public void paint(Graphics gr){
        Graphics2D g=(Graphics2D)gr;
        drawBG(g);
        drawGround(g);
        for(int i=0;i<SZ;i++){
            for(int c=0;c<SZ;c++){
                g.drawOval(getX(i,c),getY(i,c),3,3);
            }
        }
        drawBuildings(g);
    }
    private void drawBG(Graphics2D g){
        g.setColor(Color.BLACK);
        g.fillRect(0,0,Const.SIZE_W,Const.SIZE_H);
    }
    private void drawGround(Graphics2D g){
        for(int i=0;i<SZ;i++){
            for(int c=0;c<SZ;c++){
                g.drawImage(ground[i][c],getX(i,c)-(int)(CELL_SZ*rt/2),getY(i,c)-CELL_SZ/2,null);
            }
        }
        /*for(int i=0;i<World.SZ;i++){
            for(int c=0;c<World.SZ;c++){
                g.drawImage(ground2[i][c],getX(i,c)-(int)(CELL_SZ*rt/2),getY(i,c)-CELL_SZ/2,null);
            }
        }*/
    }
    private void drawBuildings(Graphics2D g){
        Vector<Building>buildings=world.getBuildings();
        for(int i=0;i<buildings.size();i++){
            g.drawImage(buildings.get(i).getImage(),getX(buildings.get(i).getX(),buildings.get(i).getY())-Const.IMAGE_W/2,getY(buildings.get(i).getX(),buildings.get(i).getY())-Const.IMAGE_H/2,null);
        }
    }
    private int getX(int x,int y){
        return (int)((x-y)*rt*CELL_SZ/2)+(int)(CELL_SZ*SZ*rt/2);
    }
    private int getY(int x,int y){
        return (x+y)*CELL_SZ/2+CELL_SZ;
    }
}
