package befunge;
import java.awt.Font;
import javax.swing.*;
import java.util.StringTokenizer;
public class InputField extends JTextArea{
    private Model model;
    public InputField(Model model){
        super();
        this.model=model;
        setFont(new Font("Courier new",Font.PLAIN,30));
    }
    
}
