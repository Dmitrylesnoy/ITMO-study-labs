package lab5.system.model.builders;

import lab5.system.io.console.StdConsole;
import lab5.system.model.Coordinates;

public class CoordinatesBuilder {
    private Double x;
    private Float y; // not null

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

    public Coordinates build() {
        if (x == null)
            return new Coordinates(y);
        else
            return new Coordinates(x, y);
    }

    public Double setX() {
        while (true) {
            try {
                return Double.parseDouble(StdConsole.read("Enter the x coordinate (double): "));
            } catch (Exception e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for x.");
            }
        }
    }

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
