package graphon;

import java.util.Random;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public abstract class Graphs {
    private static final Random random = new Random();

    public static AbstractGraph random(int vertexCount, double probability) {
        AbstractGraph graph = new ListGraph(vertexCount);
        for (int i = 0; i < vertexCount; ++i) {
            for (int j = i+1; j < vertexCount; ++j) {
                if (random.nextDouble() < probability) {
                    graph.addEdge(i, j);
                }
            }
        }
        return graph;
    }

    public static GraphColoring getComponents(AbstractGraph graph) {
        GraphColoring coloring = new GraphColoring(graph, 0);
        int currentColor = 0;
        for (int vertex = 0; vertex < graph.getVertexCount(); ++vertex) {
            if (coloring.getColorOf(vertex) == 0) {
                ++currentColor;
                dfs(graph, vertex, coloring, currentColor);
            }
        }
        return coloring;
    }

    private static void dfs(AbstractGraph graph, int vertex, GraphColoring coloring, int color) {
        coloring.setColorOf(vertex, color);
        for (int adjacent : graph.getAdjacentTo(vertex)) {
            if (coloring.getColorOf(adjacent) == 0) {
                dfs(graph, adjacent, coloring, color);
            }
        }
    }
}
