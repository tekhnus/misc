package happiness;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public class ComponentColorField implements Field<Color> {
    private final Field<Double> redComponent;
    private final Field<Double> greenComponent;
    private final Field<Double> blueComponent;

    public ComponentColorField(Field<Double> redComponent,
                               Field<Double> greenComponent,
                               Field<Double> blueComponent) {
        this.redComponent = redComponent;
        this.greenComponent = greenComponent;
        this.blueComponent = blueComponent;
    }

    @Override
    public Color valueAt(Vec2d argument) {
        return Color.temp(redComponent.valueAt(argument),
                          greenComponent.valueAt(argument),
                          blueComponent.valueAt(argument));
    }
}
