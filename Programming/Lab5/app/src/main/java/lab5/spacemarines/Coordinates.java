package lab5.spacemarines;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement
@XmlType(propOrder = { "x", "y" })
public class Coordinates {
    private double x;
    private Float y; // Поле не может быть null

    // No-argument constructor
    public Coordinates() {
    }

    public Coordinates(double x) {
        this.x = x;
    }

    public Coordinates(double x, Float y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public Float getY() {
        return y;
    }

    public void setY(Float y) {
        this.y = y;
    }

    @Override
    public String toString() {
        return this.getClass().toString() + ", " + x + ", " + y;
    }
}
