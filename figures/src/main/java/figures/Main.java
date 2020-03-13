package figures;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
public class Main extends JFrame implements ChangeListener{
    private JSlider a1,a2,dif;
    public static void main(String[] args){
        new Main();
    }
    public Main(){
        super("Figures");
        setSize(800,600);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());
        JPanel root=new JPanel();
        root.setLayout(new BoxLayout(root,BoxLayout.Y_AXIS));
        a1=new JSlider(JSlider.HORIZONTAL,1,10,3);
        a1.setMajorTickSpacing(1);
        a1.setPaintTicks(true);
        a1.addChangeListener(this);
        a2=new JSlider(JSlider.HORIZONTAL,1,10,3);
        a2.setMajorTickSpacing(1);
        a2.setPaintTicks(true);
        a2.addChangeListener(this);
        dif=new JSlider(JSlider.HORIZONTAL,0,15,0);
        dif.setMajorTickSpacing(1);
        dif.setPaintTicks(true);
        dif.addChangeListener(this);
        root.add(new JLabel("Period 1:"));
        root.add(a1);
        root.add(new JLabel("Period 2:"));
        root.add(a2);
        root.add(new JLabel("Difference:"));
        root.add(dif);
        add(root,BorderLayout.EAST);
        setVisible(true);
    }
    public void paint(Graphics g){
        super.paint(g);
        g.setColor(Color.RED);
        for(double t=0;t<10;t+=0.001){
            double x=150*Math.sin(a1.getValue()*t);
            double y=150*Math.cos(a2.getValue()*t+dif.getValue()*Math.PI/8);
            g.fillOval(toRealX(x),toRealY(y),5,5);
        }
    }
    public int toRealX(double x){
        return (int)x+400;
    }
    public int toRealY(double y){
        return (int)(300-y);
    }
    public void stateChanged(ChangeEvent e){
        repaint();
    }
}