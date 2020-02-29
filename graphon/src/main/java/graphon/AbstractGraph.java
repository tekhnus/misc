package graphon;

import java.util.List;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public interface AbstractGraph {
    public int getVertexCount();
    public boolean areAdjacent(int vertex1, int vertex2);
    public List<Integer> getAdjacentTo(int vertex);

    public void addEdge(int vertex1, int vertex2);
}
