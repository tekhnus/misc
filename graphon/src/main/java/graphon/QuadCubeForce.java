package graphon;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class QuadCubeForce implements Force {
    private final double stable;
    private final double globalFactor;
    private final int upperLimit;

    public QuadCubeForce(double stable, double globalFactor, int upperLimit) {
        this.stable = stable;
        this.globalFactor = globalFactor;
        this.upperLimit = upperLimit;
    }

    @Override
    public Vec2d force(Vec2d radius) {
        double quadrance = radius.computeQuadrance();
        if (quadrance < 1e-7 || (upperLimit > 0 && quadrance > upperLimit)) {
            return Vec2d.getZero();
        }
        Vec2d normalized = radius.normalize();
        Vec2d quadratic = normalized.multiply(quadrance);
        Vec2d cubic = normalized.multiply(1 / quadrance);
        double factor = -Math.pow(stable, 4);
        Vec2d result = quadratic.add(cubic.multiply(factor)).multiply(globalFactor);
//        System.out.println(quadratic + ", " + cubic + ", " + quadrance + ", " + result);
        return result;
    }
}
