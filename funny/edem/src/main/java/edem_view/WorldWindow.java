package edem_view;
import edem_main.Const;
import edem_world.World;
import java.awt.Color;
import javax.swing.*;
public class WorldWindow extends JFrame{
    private WorldPane content;
    private TechPane techpane;
    private JPanel root;
    public WorldWindow(){
        super("Edem");
        setSize(Const.SIZE_W+20,Const.SIZE_H+Const.SIZE_LOWER+20);
        //setResizable(false);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        root=new JPanel();
        root.setLayout(null);
        root.setBackground(Color.BLACK);
        content=new WorldPane(World.getWorld());
        root.add(content);
        techpane=new TechPane();

        root.add(techpane);
        add(root);
        setVisible(true);
    }
}
