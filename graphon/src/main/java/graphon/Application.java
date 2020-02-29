package graphon;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class Application implements ActionListener {
    private AbstractGraph graph;
    private final Force sameForce = new QuadCubeForce(0.001, 1e-7, -1);
    private final Force notSameForce = new QuadCubeForce(100, 1e-7, 10000);
    private ForceRule rule;
    private GraphView view;
    private GraphDisplay display;
    private JFrame frame;
    private final Timer timer;

    public static void main(String[] args) {
        new Application();
    }

    public Application() {

        frame = new JFrame("GraphOn demo");
        frame.setSize(1800, 1000);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(KeyEvent e) {
                reInit();
            }
        });

        reInit();
        frame.setVisible(true);

        timer = new Timer(3, this);
        timer.start();
    }

    private void reInit() {
        int n = 70;
        double p = 0.005;
        graph = Graphs.random(n, p);
        rule = new ComponentForceRule(graph, sameForce, notSameForce);
        view = new GraphView(graph, rule);
        display = new GraphDisplay(view, 1);
//        frame.removeAll();
        frame.add(display);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        this.view.makeStep();
        this.display.repaint();
    }
}
