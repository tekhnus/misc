package graphon;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class GraphView {
    private final AbstractGraph graph;
    private final ForceRule rule;
    private final int size;
    private final List<Vec2d> points;

    public GraphView(AbstractGraph graph, ForceRule rule) {
        this.graph = graph;
        this.rule = rule;
        this.size = graph.getVertexCount();
        this.points = new ArrayList<Vec2d>();

        Random random = new Random(43);
        for (int i = 0; i < size; ++i) {
            this.points.add(new Vec2d(random.nextDouble()*100, random.nextDouble()*100));
        }
    }

    public void makeStep() {
        List<Vec2d> forces = new ArrayList<Vec2d>();
        for (int i = 0; i < size; ++i) {
            Vec2d forceVector = new Vec2d();
            for (int j = 0; j < size; ++j) {
                if (i == j)
                    continue;
                Force force = rule.getForceFor(i, j);
                forceVector = forceVector.add(force.force(points.get(j).subtract(points.get(i))));
            }
            forces.add(forceVector);
        }
        for (int i = 0; i < size; ++i) {
            this.points.set(i, this.points.get(i).add(forces.get(i)));
        }
    }

    public int getSize() {
        return size;
    }

    public List<Vec2d> getPoints() {
        return points;
    }

    public AbstractGraph getGraph() {
        return graph;
    }
}
