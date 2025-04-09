package lab6.system.model.builders;

import lab6.system.io.console.StdConsole;
import lab6.system.model.Chapter;
import lab6.system.model.Coordinates;
import lab6.system.model.MeleeWeapon;
import lab6.system.model.SpaceMarine;

/**
 * SpaceMarineBuilder is responsible for constructing SpaceMarine objects.
 * This class prompts the user for various attributes of the SpaceMarine,
 * ensuring valid input for each attribute.
 */
public class SpaceMarineBuilder {
    private Long id; // Field cannot be null, must be greater than 0, unique, and generated
                     // automatically
    private String name; // Field cannot be null, cannot be empty
    private Coordinates coordinates; // Field cannot be null
    private java.util.Date creationDate; // Field cannot be null, generated automatically
    private Double health; // Must be greater than 0
    private Boolean loyal;
    private String achievements; // Field cannot be null
    private MeleeWeapon meleeWeapon; // Field can be null
    private Chapter chapter; // Field can be null

    /**
     * Default constructor for the SpaceMarineBuilder class, initializing the
     * attributes
     * of the SpaceMarine based on user input.
     */
    public SpaceMarineBuilder() {
        setName();
        setCoordinates();
        setHealth();
        setLoyalty();
        setAchievements();
        setMeleeWeapon();
        setChapter();
    }

    /**
     * Builds and returns a SpaceMarine object with the specified attributes.
     *
     * @return a new SpaceMarine object
     */
    public SpaceMarine build() {
        SpaceMarine buildMarine = new SpaceMarine(name, coordinates, achievements, meleeWeapon, chapter);
        if (health != null && health > 0)
            buildMarine.setHealth(health);
        if (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))
            buildMarine.setLoyal(loyal);
        return buildMarine;
    }

    /**
     * Prompts the user to enter the name of the SpaceMarine and validates the
     * input.
     */
    public void setName() {
        String name;
        while (true) {
            name = StdConsole.getInstance().read("Enter the name of the SpaceMarine: ");
            if (name != null && !name.isEmpty()) {
                this.name = name;
                break;
            } else {
                StdConsole.writeln("Invalid input: Name cannot be null or empty. Please try again.");
            }
        }
    }

    /**
     * Prompts the user to enter the coordinates of the SpaceMarine.
     */
    public void setCoordinates() {
        this.coordinates = new CoordinatesBuilder().build();
    }

    /**
     * Prompts the user to enter the health of the SpaceMarine and validates the
     * input.
     */
    public void setHealth() {
        Double health;
        String ans = "";
        while (ans.equals("Y") == false) {
            ans = StdConsole.getInstance().read("Would you set the health of the SpaceMarine? (Y/N)");
            if (ans.equals("N")) {
                return;
            }
        }
        while (true) {
            try {
                health = Double.parseDouble(StdConsole.getInstance().read("Enter the health of the SpaceMarine: "));
                if (health > 0) {
                    this.health = health;
                    return;
                } else {
                    StdConsole.writeln("Invalid input: Health must be greater than 0. Please try again.");
                }
            } catch (NumberFormatException e) {
                StdConsole.writeln("Invalid input: Please enter a valid number for health.");
            } catch (Exception e) {
                StdConsole.writeln(e.toString());
            }
        }
    }

    /**
     * Prompts the user to enter the loyalty status of the SpaceMarine.
     */
    public void setLoyalty() {
        String ans = "";
        while (ans.equals("Y") == false) {
            ans = StdConsole.getInstance().read("Would you set the loyalty of the SpaceMarine? (Y/N)");
            if (ans.equals("N"))
                return;
        }
        while (true) {
            ans = StdConsole.getInstance().read("Is the SpaceMarine loyal? (true/false): ");
            if (ans.equalsIgnoreCase("true")) {
                this.loyal = true;
                return;
            } else if (ans.equalsIgnoreCase("false")) {
                this.loyal = false;
                break;
            } else {
                StdConsole.writeln("Invalid input: Please enter 'true' or 'false'.");
            }
        }
    }

    /**
     * Prompts the user to enter the achievements of the SpaceMarine and validates
     * the input.
     */
    public void setAchievements() {
        String achievements;
        while (true) {
            achievements = StdConsole.getInstance().read("Enter the achievements of the SpaceMarine: ");
            if (achievements != null && !achievements.isEmpty()) {
                this.achievements = achievements;
                break;
            } else {
                StdConsole.writeln("Invalid input: Achievements cannot be null or empty. Please try again.");
            }
        }
    }

    /**
     * Prompts the user to enter the melee weapon of the SpaceMarine and validates
     * the input.
     */
    public void setMeleeWeapon() {
        String ans;
        while (true) {
            StdConsole.writeln("Enter the MeleeWeapon of the SpaceMarine: ");
            StdConsole.write("MeleeWeapon variables: \n    CHAIN_SWORD,\r\n" + //
                    "    POWER_SWORD,\r\n" + //
                    "    POWER_BLADE,\r\n" + //
                    "    POWER_FIST; \r\n");
            ans = StdConsole.getInstance().read();
            try {
                this.meleeWeapon = MeleeWeapon.valueOf(ans.toUpperCase());
                break;
            } catch (Exception e) {
                StdConsole.write("Invalid input: MeleeWeapon does not have this value. Please try again.");
            }
        }
    }

    /**
     * Prompts the user to enter the chapter of the SpaceMarine.
     */
    public void setChapter() {
        this.chapter = new ChapterBuilder().build();
    }

    public SpaceMarineBuilder setID(Long id) {
        this.id = id;
        return this;
    }
}
