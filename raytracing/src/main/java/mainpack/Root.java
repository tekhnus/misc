package mainpack;
import java.awt.Graphics;
import javax.swing.JFrame;
import rays.Color;
public class Root extends JFrame{
    public Color[][] buffer;
    public int w,h;
    public int count;
    public Root(int width,int height){
        super("RayTracer");
        w=width;
        h=height;
        buffer=new Color[w][h];
        for(int i=0;i<w;i++)for(int j=0;j<h;j++)buffer[i][j]=new Color(0,0,0);
        count=0;
        setSize(width+100,height+100);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }
    public void paint(Graphics g){
        super.paint(g);
        for(int i=0;i<w;i++){
            for(int c=0;c<h;c++){
                drawPix(i,c,buffer[i][c],g);
            }
        }
    }
    private void drawPix(int i,int j,Color c,Graphics g){
        g.setColor(new java.awt.Color((float)c.r,(float)c.g,(float)c.b));
        g.drawLine(i+30,h-j+50,i+30,h-j+50);
    }
    public void refr(int i,int j,Color c){
        buffer[i][j]=c;
        if(++count%100000==0)repaint();
    }
}
