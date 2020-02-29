package graphon;

/**
 * Project: GraphOn
 * Date: 05.06.13
 */
public class ComponentForceRule implements ForceRule {
    private final GraphColoring coloring;
    private final Force sameForce;
    private final Force notSameForce;

    public ComponentForceRule(AbstractGraph graph, Force sameForce, Force notSameForce) {
        this.coloring = Graphs.getComponents(graph);
        this.sameForce = sameForce;
        this.notSameForce = notSameForce;
    }

    @Override
    public Force getForceFor(int vertex1, int vertex2) {
        if (coloring.getColorOf(vertex1) == coloring.getColorOf(vertex2)) {
            return sameForce;
        }
        else {
            return notSameForce;
        }
    }
}
