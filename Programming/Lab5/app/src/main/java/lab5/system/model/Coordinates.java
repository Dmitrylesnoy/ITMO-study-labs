package lab5.system.model;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

@JacksonXmlRootElement(localName = "Coordinates")
public class Coordinates {
    @JacksonXmlElementWrapper(localName = "x")
    private double x;
    @JacksonXmlElementWrapper(localName = "y")
    private Float y; // Поле не может быть null

    public Coordinates() {
    }

    public Coordinates(double x) {
        this.x = x;
    }

    public Coordinates(double x, Float y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int hashCode() {
        int result = 1;
        long temp = Double.doubleToLongBits(x);
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        result = 31 * result + ((y == null) ? 0 : y.hashCode());
        return result;
    }

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
            if (other.y != null)
                return false;
        } else if (!y.equals(other.y))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return this.getClass().toString() + ", " + x + ", " + y;
    }
}
