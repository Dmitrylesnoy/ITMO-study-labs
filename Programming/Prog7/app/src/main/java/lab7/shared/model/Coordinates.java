package lab7.shared.model;

import java.io.Serializable;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a set of coordinates with X and Y values.
 * This class provides constructors for initializing the coordinates,
 * methods for generating hash codes, equality checks, and string
 * representation.
 */
@Getter
@Setter
@JacksonXmlRootElement(localName = "Coordinates")
public class Coordinates implements Serializable {
    @JacksonXmlElementWrapper(localName = "x")
    private double x;
    @JacksonXmlElementWrapper(localName = "y")
    private Float y; // Field cannot be null

    /**
     * Default constructor for the Coordinates class.
     */
    public Coordinates() {
    }

    /**
     * Constructs a Coordinates object with the specified X value.
     *
     * @param x the X coordinate
     */
    public Coordinates(double x) {
        this.x = x;
    }

    /**
     * Constructs a Coordinates object with the specified X and Y values.
     *
     * @param x the X coordinate
     * @param y the Y coordinate
     */
    public Coordinates(double x, Float y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Returns the hash code for the Coordinates object.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        int result = 1;
        long temp = Double.doubleToLongBits(x);
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        result = 31 * result + ((y == null) ? 0 : y.hashCode());
        return result;
    }

    /**
     * Checks if this Coordinates object is equal to another object.
     *
     * @param obj the object to compare
     * @return true if equal, false otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Coordinates other = (Coordinates) obj;
        if (Double.doubleToLongBits(x) != Double.doubleToLongBits(other.x))
            return false;
        if (y == null) {
            return other.y == null;
        } else {
            return y.equals(other.y);
        }
    }

    /**
     * Returns a string representation of the Coordinates object.
     *
     * @return a string description of the Coordinates
     */
    @Override
    public String toString() {
        return this.getClass().toString() + ", " + x + ", " + y;
    }
}
