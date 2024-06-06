package befunge;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
public class EditFrame extends JFrame implements ActionListener{
    private Model model;
    private JPanel root;
    private InputField field;
    private JButton runMode;
    private RunFrame fr;
    public void addFriend(RunFrame fr){
        this.fr=fr;
    }
    public EditFrame(Model model){
        super("Just simple Befunge interpreter.");
        this.model=model;
        this.fr=fr;
        setSize(1000,800);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
        root=new JPanel();
        root.setLayout(new BorderLayout());
        field=new InputField(this.model);
        field.setText(">v\n^<");
        root.add(field,BorderLayout.CENTER);
        runMode=new JButton("Run Mode");
        runMode.addActionListener(this);
        root.add(runMode,BorderLayout.SOUTH);
        add(root);
        revalidate();
        repaint();
    }
    public void actionPerformed(ActionEvent e){
        if(e.getActionCommand().equals("Run Mode")){
            model.setProg(field.getText());
            model.updateModel();
            invis();
            fr.vis();
        }
    }
    public void invis(){
        setVisible(false);
    }
    public void vis(){
        setVisible(true);
    }
}
