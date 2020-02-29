package graphon;

import java.util.ArrayList;
import java.util.List;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class ListGraph implements AbstractGraph {
    private final int vertexCount;
    private final List<List<Integer>> adjacent;

    public ListGraph(int vertexCount) {
        this.vertexCount = vertexCount;
        this.adjacent = new ArrayList<List<Integer>>();
        for (int i = 0; i < this.vertexCount; ++i) {
            this.adjacent.add(new ArrayList<Integer>());
        }
    }

    @Override
    public int getVertexCount() {
        return vertexCount;
    }

    @Override
    public boolean areAdjacent(int vertex1, int vertex2) {
        return this.adjacent.get(vertex1).contains(vertex2);
    }

    @Override
    public List<Integer> getAdjacentTo(int vertex) {
        return this.adjacent.get(vertex);
    }

    @Override
    public void addEdge(int vertex1, int vertex2) {
        this.adjacent.get(vertex1).add(vertex2);
        this.adjacent.get(vertex2).add(vertex1);
    }
}
