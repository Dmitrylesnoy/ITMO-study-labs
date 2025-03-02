package lab5.io;

import java.util.Scanner;

import lab5.spacemarines.Chapter;
import lab5.spacemarines.Coordinates;
import lab5.spacemarines.MeleeWeapon;
import lab5.spacemarines.SpaceMarine;

public class SpaceMarineBuilder {

    private Scanner scanner;

    private Long id; // Поле не может быть null, Значение поля должно быть больше 0, Значение этого
                     // поля должно быть уникальным, Значение этого поля должно генерироваться
                     // автоматически
    private String name; // Поле не может быть null, Строка не может быть пустой
    private Coordinates coordinates; // Поле не может быть null
    private java.util.Date creationDate; // Поле не может быть null, Значение этого поля должно генерироваться
                                         // автоматически
    private double health; // Значение поля должно быть больше 0
    private boolean loyal;
    private String achievements; // Поле не может быть null
    private MeleeWeapon meleeWeapon; // Поле может быть null
    private Chapter chapter; // Поле может быть null

    public SpaceMarineBuilder() {
        this.scanner = new Scanner(System.in);
    }

    public SpaceMarine build() {
        if (health > 0 && (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))) {
            return new SpaceMarine(name, coordinates, health, loyal, achievements, meleeWeapon, chapter);
        } else {
            return new SpaceMarine(name, coordinates, achievements, meleeWeapon, chapter);
        }
    }

    public String setName() {
        String name;
        while (true) {
            System.out.print("Enter the name of the SpaceMarine: ");
            name = scanner.nextLine();
            if (name != null && !name.isEmpty()) {
                this.name = name;
            } else {
                System.out.println("Invalid input: Name cannot be null or empty. Please try again.");
            }
        }
    }

    public void setCoordinates() {
        Float y = setCoordinateY();

        System.out.print("Would you enter the X coordinate? Y/N: ");
        if (scanner.nextLine() == "Y") {
            double x = setCoordinateX();
            this.coordinates = new Coordinates(x, y);

        } else {
            this.coordinates = new Coordinates(y);
        }
    }

    public double setCoordinateX() {
        double x;
        while (true) {
            System.out.print("Enter the x coordinate: ");
            try {
                x = Double.parseDouble(scanner.nextLine());
                return x;
            } catch (NumberFormatException e) {
                System.out.println("Invalid input: Please enter a valid number for x.");
            }
        }
    }

    public Float setCoordinateY() {
        Float y;
        while (true) {
            System.out.print("Enter the y coordinate: ");
            try {
                y = Float.parseFloat(scanner.nextLine());
                return y;
            } catch (NumberFormatException e) {
                System.out.println("Invalid input: Please enter a valid number for y.");
            }
        }
    }

    public void setHealth() {
        double health;
        while (true) {
            System.out.print("Enter the health of the SpaceMarine: ");
            try {
                health = Double.parseDouble(scanner.nextLine());
                if (health > 0) {
                    this.health = health;
                    break;
                } else {
                    System.out.println("Invalid input: Health must be greater than 0. Please try again.");
                }
            } catch (NumberFormatException e) {
                System.out.println("Invalid input: Please enter a valid number for health.");
            }
        }
    }

    public void setLoyalty() {
        String input;
        while (true) {
            System.out.print("Is the SpaceMarine loyal? (true/false): ");
            input = scanner.nextLine();
            if (input.equalsIgnoreCase("true")) {
                this.loyal = true;
                break;
            } else if (input.equalsIgnoreCase("false")) {
                this.loyal = false;
                break;
            } else {
                System.out.println("Invalid input: Please enter 'true' or 'false'.");
            }
        }
    }

    public void setAchievements() {
        String achievements;
        while (true) {
            System.out.print("Enter the achievements of the SpaceMarine: ");
            achievements = scanner.nextLine();
            if (achievements != null && !achievements.isEmpty()) {
                this.achievements = achievements;
                break;
            } else {
                System.out.println("Invalid input: Achievements cannot be null or empty. Please try again.");
            }
        }
    }

    public void setMeleeWeapon() {
        String meleeWeapon;
        while (true) {
            System.out.println("Enter the MeleeWeapon of the SpaceMarine: ");
            System.out.print("MeleeWeapon variables:     CHAIN_SWORD,\r\n" + //
                    "    POWER_SWORD,\r\n" + //
                    "    POWER_BLADE,\r\n" + //
                    "    POWER_FIST; ");
            meleeWeapon = scanner.nextLine();
            try {
                this.meleeWeapon = MeleeWeapon.valueOf(meleeWeapon);
                break;
            } catch (Exception e) {
                System.out.println("Invalid input: MeleeWeapon have not this value. Please try again.");
            }
        }
    }

    public void setChapter() {
        String name;
        String world;
        while (true) {
            System.out.print("Enter the Name of the Chapter: ");
            name = scanner.nextLine();
            if (name != null && !name.isEmpty()) {
                break;
            } else {
                System.out.println("Invalid input: Name cannot be null or empty. Please try again.");
            }
        }
        while (true) {
            System.out.print("Enter the World of the Chapter: ");
            world = scanner.nextLine();
            if (world != null) {
                this.chapter = new Chapter(name, world);
                break;
            } else {
                System.out.println("Invalid input: World cannot be null. Please try again.");
            }
        }
    }
}
