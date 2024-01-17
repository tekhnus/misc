package happiness;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public class Color {
    private static final Color temp = new Color(0, 0, 0);
    private double red, green, blue;

    private Color(double red, double green, double blue) {
        this.red = red;
        this.green = green;
        this.blue = blue;
    }

    public int toRGBInt() {
        return ((int) (red * 255) << 16) + ((int) (green * 255) << 8) + (int) (blue * 255);
    }

    public static Color temp(double red, double green, double blue) {
        temp.red = red;
        temp.green = green;
        temp.blue = blue;
        return temp;
    }
}
