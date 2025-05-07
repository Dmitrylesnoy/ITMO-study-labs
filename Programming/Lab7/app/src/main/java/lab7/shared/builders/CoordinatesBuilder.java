package lab7.shared.builders;

import lab7.shared.io.console.StdConsole;
import lab7.shared.model.Coordinates;

/**
 * CoordinatesBuilder is responsible for constructing Coordinates objects.
 * This class prompts the user for the Y coordinate (which cannot be null)
 * and optionally for the X coordinate, ensuring valid input.
 */
public class CoordinatesBuilder {
    private Double x;
    private Float y; // not null

    /**
     * Default constructor for the CoordinatesBuilder class, initializing the Y coordinate
     * and optionally the X coordinate based on user input.
     */
    public CoordinatesBuilder() {
        this.y = setY();
        while (true) {
            String ans = StdConsole.read("Would you enter the X coordinate? Y/N:");
            if (ans.equals("Y")) {
                this.x = setX();
                break;
            } else if (ans.equals("N")) {
                break;
            }
        }
    }

    /**
     * Builds and returns a Coordinates object with the specified X and Y
     * coordinates.
     *
     * @return a new Coordinates object
     */
    public Coordinates build() {
        if (x == null)
            return new Coordinates(y);
        else
            return new Coordinates(x, y);
    }

    /**
     * Prompts the user to enter the X coordinate and validates the input.
     *
     * @return the X coordinate as a Double
     */
    public Double setX() {
        while (true) {
            try {
                return Double.parseDouble(StdConsole.read("Enter the x coordinate (double): "));
            } catch (Exception e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for x.");
            }
        }
    }

    /**
     * Prompts the user to enter the Y coordinate and validates the input.
     *
     * @return the Y coordinate as a Float
     */
    public Float setY() {
        while (true) {
            try {
                return Float.parseFloat(StdConsole.read("Enter the y coordinate (float): "));
            } catch (Exception e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for y.");
            }
        }
    }
}
