package harius.snowflakes;

import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;
import javax.swing.JFrame;
import mikera.vectorz.*;

/**
 * Hello world!
 *
 */
public class App extends TimerTask
{
    private World world;
    private Sound sound;
    private Random random = new Random();
    private JFrame frame = new JFrame("Snow!");
    private View view;
    
    private static final long RATE = 10;
    private static final double WORLD_WIDTH = 50.0, WORLD_HEIGHT = 25.0;
    
    public static void main( String[] args )
    {
        new App().runApp();
    }
    
    public void runApp() {
        frame.setSize(1200, 600);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setUndecorated(true);
        frame.setLocationRelativeTo(null);
        world = new World(WORLD_WIDTH, WORLD_HEIGHT, Vector2.of(0.0, -9.8));
        sound = new Sound(world);
        
        view = new View(world, 24);
        frame.add(view);
        frame.setVisible(true);
        
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(this, 0, App.RATE);
    }

    @Override
    public void run() {
        for(int i = 0; i < random.nextDouble() * 1.05 - 1; ++i) {
            double x = world.getWidth() * random.nextDouble();
            double y = 1.03 * world.getHeight();
            double massFactor = (1 + 0.1 * random.nextDouble());
            world.addFlake(new Flake(world, sound, Vector2.of(x, y), 0.001 * massFactor));
        }
        world.tick(App.RATE);
        view.repaint();// FIXME: concurrency 
    }
}
