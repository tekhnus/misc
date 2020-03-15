package befunge;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.BadLocationException;
public class InOutPanel extends JPanel implements ActionListener{
    private JTextArea terminal;
    private JTextField input;
    private static InOutPanel inst=new InOutPanel();
    private int mode;//0-nothing,1-int,2-char
    private InOutPanel(){
        super();
        setLayout(new BorderLayout());
        terminal=new JTextArea(10,10);
        terminal.setEditable(false);
        add(terminal,BorderLayout.CENTER);
        input=new JTextField();
        input.addActionListener(this);
        add(input,BorderLayout.SOUTH);
        mode=0;
    }
    public static InOutPanel getConsole(){
        return inst;
    }
    public void append(String s){
        terminal.append(s);
    }
    public void actionPerformed(ActionEvent e) {
        if(mode==1){
            String inp=input.getText();
            input.setText("");
            int n=Integer.parseInt(inp);
            MyStream.getStream().push(n);
        }
        else if(mode==2){
            String inp=input.getText();
            input.setText("");
            int n=inp.charAt(0);
            MyStream.getStream().push(n);
        }
    }
    public void setMode(int newMode){
        mode=newMode;
    }
    public void clear(){
        terminal.setText("");
    }
}
