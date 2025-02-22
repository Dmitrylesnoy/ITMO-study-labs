package lab5.spacemarines;

// @XmlRootElement
// @XmlType(propOrder = { "x", "y" })
public class Coordinates {
    private double x;
    private Float y; // Поле не может быть null

    public Coordinates(double x) {
        this.x = x;
    }

    public Coordinates(double x, Float y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return this.getClass().toString() + ", " + x + ", " + y;
    }
}
