package happiness;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public class Application implements ActionListener {
    private final FieldDisplay display;
    private double time;

    public static void main(String[] args) {
        new Application();
    }

    public Application() {
        JFrame frame = new JFrame("Happiness");
        int width = 350;
        int height = 350;
        frame.setSize(width, height);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        time = 0;

        Field<Double> red = new Field<Double>() {
            @Override
            public Double valueAt(Vec2d argument) {
                return Math.pow(Math.sin(time + argument.getX()/100 + argument.getY()/200), 2);
            }
        };
        Field<Double> green = new Field<Double>() {
            @Override
            public Double valueAt(Vec2d argument) {
                return Math.pow(Math.sin(2 * time + argument.getX() * argument.getY()/10), 2);
            }
        };
        Field<Double> blue = new Field<Double>() {
            @Override
            public Double valueAt(Vec2d argument) {
                return Math.pow(Math.sin(5 * time + argument.getX()/300 + argument.getY()/400), 2);
            }
        };

        Field<Color> field = new ComponentColorField(red, green, blue);

        display = new FieldDisplay(field, width, height);
        frame.add(display);

        frame.setVisible(true);
        Timer timer = new Timer(30, this);
        timer.start();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        time += 0.1;
        display.repaint();
    }
}
