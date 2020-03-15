package befunge;
import java.awt.*;
import java.awt.event.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
public class RunFrame extends JFrame implements ActionListener,ChangeListener{
    private View view;
    private JPanel root;
    private JButton step;
    private JButton reset;
    private JPanel bottom;
    private Model model;
    private JButton editMode;
    private EditFrame fr;
    private JButton run;
    private JButton pause;
    private InOutPanel inOut;
    private JPanel down;
    private JSlider speed;
    private Thread thr;
    public void addFriend(EditFrame fr){
        this.fr=fr;
    }
    public RunFrame(Model model){
        super("Just simple Befunge interpreter.");
        this.model=model;
        setSize(1000,800);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        root=new JPanel();
        view=new View(this.model);
        root.setLayout(new BorderLayout());
        root.add(new JScrollPane(view),BorderLayout.CENTER);
        bottom=new JPanel();
        bottom.setLayout(new BoxLayout(bottom,BoxLayout.Y_AXIS));
        step=new JButton("Step over");
        step.addActionListener(this);
        bottom.add(step);
        reset=new JButton("Reset");
        reset.addActionListener(this);
        bottom.add(reset);
        run=new JButton("Run");
        run.addActionListener(this);
        bottom.add(run);
        pause=new JButton("Pause");
        pause.addActionListener(this);
        bottom.add(pause);
        speed=new JSlider(JSlider.HORIZONTAL,0,5,0);
        speed.setMajorTickSpacing(1);
        speed.setMinorTickSpacing(1);
        speed.setPaintTicks(true);
        speed.setPaintLabels(true);
        speed.addChangeListener(this);
        bottom.add(speed);
        root.add(bottom,BorderLayout.EAST);
        down=new JPanel();
        down.setLayout(new BorderLayout());
        inOut=InOutPanel.getConsole();
        JScrollPane scroll=new JScrollPane(inOut);
        down.add(scroll,BorderLayout.CENTER);
        editMode=new JButton("Edit Mode");
        editMode.addActionListener(this);
        down.add(editMode,BorderLayout.SOUTH);
        root.add(down,BorderLayout.SOUTH);
        add(root);
        thr=new Thread(model);
    }
    public void actionPerformed(ActionEvent e){
        if(e.getActionCommand().equals("Step over")){
            model.step();
        }
        else if(e.getActionCommand().equals("Reset")){
            model.reset();
            model.updateModel();
            InOutPanel.getConsole().clear();
        }
        else if(e.getActionCommand().equals("Edit Mode")){
            invis();
            fr.vis();
        }
        else if(e.getActionCommand().equals("Run")){
            thr=new Thread(model);
            thr.start();
        }
        else if(e.getActionCommand().equals("Pause")){
        }
    }
    public void invis(){
        setVisible(false);
    }
    public void vis(){
        setVisible(true);
    }

    public void stateChanged(ChangeEvent e){
        model.setWait(speed.getValue()*speed.getValue()*20);
    }
}
