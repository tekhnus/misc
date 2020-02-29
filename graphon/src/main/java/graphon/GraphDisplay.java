package graphon;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class GraphDisplay extends JPanel {
    private static final int radius = 4;
    private final GraphView view;
    private final int scale;

    public GraphDisplay(GraphView view, int scale) {
        this.view = view;
        this.scale = scale;
        this.setBackground(Color.BLACK);
    }

    private int getX(int i) {
        return this.scale * ((int) view.getPoints().get(i).getX()) + this.getWidth() / 2;
    }

    private int getY(int i) {
        return this.scale * ((int) view.getPoints().get(i).getY()) + this.getHeight() / 2;
    }

    @Override
    public void paint(Graphics g) {
        super.paint(g);
        g.setColor(Color.GREEN);
        for (int i = 0; i < view.getSize(); ++i) {
            for (int j : view.getGraph().getAdjacentTo(i)) {
                g.drawLine(getX(i), getY(i), getX(j), getY(j));
            }
        }

        g.setColor(Color.ORANGE);
        for (int i = 0; i < view.getSize(); ++i) {
            g.fillOval(getX(i) - radius, getY(i) - radius, 2*radius, 2*radius);
        }
    }
}
