package befunge;
import java.awt.*;
import java.util.Observable;
import java.util.Observer;
import javax.swing.*;
public class View extends JPanel implements Observer{
    private char data[][];
    private static final int size=20;
    private int x,y;
    private Model model;
    public void update(Observable o,Object arg){
        Model model=(Model)o;
        for(int i=0;i<Model.width;i++){
            for(int j=0;j<Model.height;j++){
                data[i][j]=model.getChar(i,j);
            }
        }
        x=model.getX();
        y=model.getY();
        repaint();
    }
    public View(Model model){
        super();
        this.model=model;
        this.model.addObserver(this);
        data=new char[Model.width][Model.height];
        setFont(new Font("Courier new",Font.BOLD,20));
    }
    public void paint(Graphics gr){
        super.paint(gr);
        Graphics2D g=(Graphics2D)gr;
        for(int i=0;i<Model.width;i++){
            for(int j=0;j<Model.height;j++){
                if(i==x&&j==y){
                    g.setColor(Color.ORANGE);
                    g.fillRect(i*size,j*size,size,size);
                }
                g.setColor(Color.BLACK);
                g.drawRect(i*size,j*size,size,size);
                g.drawString(data[i][j]+"",i*size+3,(j+1)*size-3);
            }
        }
        g.setColor(Color.BLACK);
        g.drawString("stack: "+model.getStack(),0,Model.height*(size+1));
    }
}
