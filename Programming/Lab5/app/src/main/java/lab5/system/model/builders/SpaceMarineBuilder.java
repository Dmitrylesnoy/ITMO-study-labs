package lab5.system.model.builders;

import lab5.system.io.console.StdConsole;
import lab5.system.model.Chapter;
import lab5.system.model.Coordinates;
import lab5.system.model.MeleeWeapon;
import lab5.system.model.SpaceMarine;

public class SpaceMarineBuilder {
    private Long id; // Поле не может быть null, Значение поля должно быть больше 0, Значение этого
                     // поля должно быть уникальным, Значение этого поля должно генерироваться
                     // автоматически
    private String name; // Поле не может быть null, Строка не может быть пустой
    private Coordinates coordinates; // Поле не может быть null
    private java.util.Date creationDate; // Поле не может быть null, Значение этого поля должно генерироваться
                                         // автоматически
    private Double health; // Значение поля должно быть больше 0
    private Boolean loyal;
    private String achievements; // Поле не может быть null
    private MeleeWeapon meleeWeapon; // Поле может быть null
    private Chapter chapter; // Поле может быть null

    public SpaceMarineBuilder() {
        setName();
        setCoordinates();
        setHealth();
        setLoyalty();
        setAchievements();
        setMeleeWeapon();
        setChapter();
    }

    public SpaceMarine build() {
        SpaceMarine buildMarine = new SpaceMarine(name, coordinates, achievements, meleeWeapon, chapter);
        if (health != null && health > 0)
            buildMarine.setHealth(health);
        if (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))
            buildMarine.setLoyal(loyal);
        return buildMarine;
    }

    public void setName() {
        String name;
        while (true) {
            name = StdConsole.read("Enter the name of the SpaceMarine: ");
            if (name != null && !name.isEmpty()) {
                this.name = name;
                break;
            } else {
                StdConsole.writeln("Invalid input: Name cannot be null or empty. Please try again.");
            }
        }
    }

    public void setCoordinates() {
        this.coordinates = new CoordinatesBuilder().build();
    }

    public void setHealth() {
        Double health;
        String ans = "";
        while (ans.equals("Y") == false) {
            ans = StdConsole.read("Would you set the health of the SpaceMarine? (Y/N)");
            if (ans.equals("N")) {
                return;
            }
        }
        while (true) {
            try {
                health = Double.parseDouble(StdConsole.read("Enter the health of the SpaceMarine: "));
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

    public void setLoyalty() {
        String ans = "";
        while (ans.equals("Y") == false) {
            ans = StdConsole.read("Would you set the loyalty of the SpaceMarine? (Y/N)");
            if (ans.equals("N")) return;
        }
        while (true) {
            ans = StdConsole.read("Is the SpaceMarine loyal? (true/false): ");
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

    public void setAchievements() {
        String achievements;
        while (true) {
            achievements = StdConsole.read("Enter the achievements of the SpaceMarine: ");
            if (achievements != null && !achievements.isEmpty()) {
                this.achievements = achievements;
                break;
            } else {
                StdConsole.writeln("Invalid input: Achievements cannot be null or empty. Please try again.");
            }
        }
    }

    public void setMeleeWeapon() {
        String ans;
        while (true) {
            StdConsole.writeln("Enter the MeleeWeapon of the SpaceMarine: ");
            StdConsole.write("MeleeWeapon variables: \n    CHAIN_SWORD,\r\n" + //
                    "    POWER_SWORD,\r\n" + //
                    "    POWER_BLADE,\r\n" + //
                    "    POWER_FIST; \r\n");
            ans = StdConsole.read();
            try {
                this.meleeWeapon = MeleeWeapon.valueOf(ans.toUpperCase());
                break;
            } catch (Exception e) {
                StdConsole.write("Invalid input: MeleeWeapon have not this value. Please try again.");
            }
        }
    }

    public void setChapter() {
        this.chapter = new ChapterBuilder().build();
    }
}
