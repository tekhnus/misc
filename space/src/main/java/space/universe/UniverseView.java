package space.universe;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;
import java.util.Random;
import javax.swing.JPanel;
import space.graphics.GraphicsAWT;
import space.math.MathUtils;
import space.math.Matr2;
import space.math.Vec2;

/**
 * Universe viewer panel
 */

public class UniverseView extends JPanel implements Observer, KeyListener
{
    private Universe un_;
    private Matr2 conv;
    private Vec2 cent;
    private Vec2 ucent;
    private double angle;
    private double scale;
    
    public final static double step_default = 1e10;//initial moving step size(km)
    public final static double sun_default = 8;//initial Sun size(pix)
    public final static double sun_actual = 7e8;//actual Sun size(m)
    public final static double unit_default = 8000;//initial astronomic unit size(pix)
    public final static double unit_actual = 1e13;//actual astronomic unit size(m)
    public final static Color bg = new Color(0, 0, 5);
    public final static double scale_default = unit_default / unit_actual;
    
    public final static double step_correct = step_default * scale_default;
    
    private int fixAt;
    
    private GraphicsAWT gr;
    
    /**
     * Recalculate conversion matrix
     */
    
    public void fixAt(int index)
    {
        fixAt = index;
        updUcenter();
    }
    
    private void updUcenter()
    {
        if(fixAt != -1)
        {
            ucent = new Vec2(un_.getBodies().get(fixAt).getCoord());
        }
        else
        {
            ucent = new Vec2(0, 0);
        }
        
    }
    
    private void updCenter()
    {
        cent = new Vec2(getWidth()/3, getHeight()/2);
    }
    
    private void calcConv()
    {
        conv = MathUtils.mul(MathUtils.scale(scale), MathUtils.rot(angle));
    }
    
    /**
     * Main constructor
     * @param un Universe to paint
     * @param w View width
     * @param h View height
     */
    
    public UniverseView(Universe un, int w, int h)
    {
        super();
        un.addObserver(this);
        un_ = un;
        angle = 0;
        scale = scale_default;
        calcConv();
        setBackground(bg);
        setSize(w, h);
        fixAt = -1;
    }
    
    @Override
    public void paint(Graphics g)
    {
        gr = new GraphicsAWT(g);
        super.paint(g);
        ArrayList<Body> bodies = un_.getBodies();
        updCenter();
        for(int i = 0; i < bodies.size(); i++)
        {
            if(scale / scale_default < bodies.get(i).getMinscale())
                continue;
            updUcenter();
            Vec2 crd = new Vec2(cent);
            Vec2 ucrd = new Vec2(bodies.get(i).getCoord());
            ucrd.sub(ucent);
            crd.add(MathUtils.mul(conv, ucrd));
            bodies.get(i).paint(gr, (int)crd.get(0), (int)crd.get(1), scale);
        }
        gr.setColor(Color.WHITE);
        gr.drawString("W/S -- scale", getWidth() - 100, getHeight() - 70);
        gr.drawString("A/D -- time", getWidth() - 100, getHeight() - 55);
        gr.drawString("Q/E -- rotate", getWidth() - 100, getHeight() - 40);
        gr.drawString("Shift -- focus", getWidth() - 100, getHeight() - 25);
    }

    @Override
    public void update(Observable o, Object arg)
    {
        //repaint();
        updateUI();
    }

    @Override
    public void keyTyped(KeyEvent e){}

    @Override
    public void keyPressed(KeyEvent e)
    {
        switch(e.getKeyCode())
        {
            case KeyEvent.VK_W:
                if(scale / scale_default <= 600)
                {
                    scale *= 1.07;
                    calcConv();
                }
                break;
            case KeyEvent.VK_S:
                scale /= 1.07;
                calcConv();
                break;
            case KeyEvent.VK_Q:
                angle += java.lang.Math.PI/180;
                calcConv();
                break;
            case KeyEvent.VK_E:
                angle -= java.lang.Math.PI/180;
                calcConv();
                break;
            case KeyEvent.VK_A:
                un_.step /= 1.05;
                break;
            case KeyEvent.VK_D:
                un_.step *= 1.05;
                break;
            /*case KeyEvent.VK_U:
                ucent.add(new Vec2(0, step_correct / scale));
                updCenter();
                break;
            case KeyEvent.VK_J:
                ucent.add(new Vec2(0, -step_correct / scale));
                updCenter();
                break;
            case KeyEvent.VK_H:
                ucent.add(new Vec2(step_correct / scale, 0));
                updCenter();
                break;
            case KeyEvent.VK_K:
                ucent.add(new Vec2(-step_correct / scale, 0));
                updCenter();
                break;*/
            case KeyEvent.VK_SHIFT:
                do
                {
                    fixAt++;
                    if(fixAt >= un_.getBodies().size())
                        fixAt = 0;
                }
                while(!un_.getBodies().get(fixAt).getFocus());
                break;
        }
    }

    @Override
    public void keyReleased(KeyEvent e){}
}
