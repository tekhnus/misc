package edem_view;
import edem_main.Const;
import edem_world.Tech;
import edem_world.Tech.Node;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import javax.swing.JButton;
import javax.swing.JPanel;
public class TechPane extends JPanel implements Observer,ActionListener{
    private Vector<JButton> buttons;
    private Vector<Integer> draw;
    private void updateButtons(){
        draw=Tech.getTech().getExplorable();
        this.removeAll();
        for(int i=0;i<draw.size();i++){
            this.add(buttons.get(draw.get(i)));
        }
        this.updateUI();
    }
    public TechPane(){
        buttons=new Vector<JButton>();
        Tech.getTech().addObserver(this);
        this.setBounds(Const.SIZE_W-Const.SIZE_PANE,0,Const.SIZE_PANE,Const.SIZE_H);
        setBackground(Color.BLACK);
    }
    public void update(Observable o,Object arg){
        if(arg==null){
            updateButtons();
        }
        else{
            Node newNode=(Node)arg;
            JButton tmp=new JButton(newNode.getName());
            tmp.setActionCommand(newNode.getNumber()+"");
            tmp.addActionListener(this);
            tmp.setBackground(Color.BLACK);
            tmp.setForeground(Color.WHITE);
            tmp.setFont(new Font("Courier new",Font.PLAIN,15));
            buttons.add(tmp);
            updateButtons();
        }
    }

    public void actionPerformed(ActionEvent e){
        int number=Integer.parseInt(e.getActionCommand());
        Tech.getTech().exploreNode(number);
    }
}
