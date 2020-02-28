package happiness;

/**
 * Project: Happiness
 * Date: 26.05.13
 */
public interface Operator <ArgumentType, ValueType> {
    ValueType valueAt(ArgumentType argument);
}
