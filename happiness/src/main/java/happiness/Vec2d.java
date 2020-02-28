package happiness;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public class Vec2d {
    private final static Vec2d temp = new Vec2d(0, 0);

    private double x;
    private double y;

    private Vec2d(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public static Vec2d temp(double x, double y) {
        temp.x = x;
        temp.y = y;
        return temp;
    }
}
