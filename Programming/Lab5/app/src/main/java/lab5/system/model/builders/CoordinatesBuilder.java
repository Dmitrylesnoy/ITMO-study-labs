package lab5.system.model.builders;

import lab5.system.io.Console.StdConsole;
import lab5.system.model.Coordinates;

public class CoordinatesBuilder {
    ;
    private Double x;
    private Float y;

    public CoordinatesBuilder() {

        if (StdConsole.read("Would you enter the X coordinate? Y/N: ") == "Y") {
            this.x = setX();
            this.y = setY();

        } else {
            this.y = setY();
        }
    }

    public Coordinates build() {
        return new Coordinates(x, y);
    }

    public double setX() {
        double x;
        while (true) {
            StdConsole.write("Enter the x coordinate: ");
            try {
                x = Double.parseDouble(StdConsole.read());
                return x;
            } catch (NumberFormatException e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for x.");
            }
        }
    }

    public Float setY() {
        Float y;
        while (true) {
            StdConsole.write("Enter the y coordinate: ");
            try {
                y = Float.parseFloat(StdConsole.read());
                return y;
            } catch (NumberFormatException e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for y.");
            }
        }
    }
}
