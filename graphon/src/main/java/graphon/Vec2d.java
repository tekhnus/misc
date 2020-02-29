package graphon;

/**
 * Project: GraphOn
 * Date: 27.05.13
 */
public class Vec2d {
    private final double x;
    private final double y;

    private final static Vec2d zero = new Vec2d();

    public Vec2d(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Vec2d() {
        this(0, 0);
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public Vec2d add(Vec2d another) {
        return new Vec2d(this.x + another.x, this.y + another.y);
    }

    public Vec2d subtract(Vec2d another) {
       return new Vec2d(this.x - another.x, this.y - another.y);
    }

    public Vec2d multiply(double factor) {
        return new Vec2d(factor*x, factor*y);
    }

    public Vec2d normalize() {
        return this.multiply(1 / Math.sqrt(this.computeQuadrance()));
    }

    public double computeQuadrance() {
        return x*x + y*y;
    }

    @Override
    public String toString() {
        return "(" + this.x + "; " + this.y + ")";
    }

    public static Vec2d getZero() {
        return zero;
    }
}
