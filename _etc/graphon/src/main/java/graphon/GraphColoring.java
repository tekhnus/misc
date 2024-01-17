package graphon;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Project: GraphOn
 * Date: 05.06.13
 */
public class GraphColoring {
    private final AbstractGraph graph;
    private final List<Integer> colors;

    public GraphColoring(AbstractGraph graph, int initialColor) {
        this.graph = graph;
        this.colors = new ArrayList<Integer>();
        for (int i = 0; i < graph.getVertexCount(); ++i) {
            this.colors.add(initialColor);
        }
    }

    public int getColorOf(int vertex) {
        return colors.get(vertex);
    }

    public void setColorOf(int vertex, int newColor) {
        colors.set(vertex, newColor);
    }
}
