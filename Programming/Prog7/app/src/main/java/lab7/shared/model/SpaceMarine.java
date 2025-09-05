package lab7.shared.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lab7.shared.io.console.StdConsole;
import lombok.Getter;
import lombok.Setter;

/**
 * Represents a SpaceMarine entity with various attributes such as id, name,
 * coordinates, health, loyalty, achievements, melee weapon, and chapter.
 * This class provides constructors for initializing these attributes and
 * methods for generating hash codes, equality checks, and string
 * representation.
 */
@Getter
@Setter
@JacksonXmlRootElement(localName = "SpaceMarine")
public class SpaceMarine implements Comparable<SpaceMarine>, Serializable {
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
    @JsonIgnore
    private transient StdConsole console = new StdConsole();
    private Integer creator_id;

    /**
     * Default constructor for the SpaceMarine class.
     */
    public SpaceMarine() {
    }

    /**
     * Constructs a SpaceMarine with the specified attributes.
     *
     * @param name the name of the SpaceMarine
     * @param coordinates the coordinates of the SpaceMarine
     * @param achievements the achievements of the SpaceMarine
     * @param meleeWeapon the melee weapon of the SpaceMarine
     * @param chapter the chapter of the SpaceMarine
     */
    public SpaceMarine(String name, Coordinates coordinates, String achievements, MeleeWeapon meleeWeapon,
            Chapter chapter) {
        try {
            if (name != null && name == "") {
                throw new IllegalAccessError("Name mustn't be empty String");
            } else if (name != null && coordinates != null && achievements != null) {
                this.name = name;
                this.coordinates = coordinates;
                this.creationDate = new java.util.Date();
                this.achievements = achievements;
                this.meleeWeapon = meleeWeapon;
                this.chapter = chapter;
                // this.id = IDgenerator.getNextId();
            } else
                throw new IllegalArgumentException("Argument mustn't be null");
        } catch (Exception e) {
            console.writeln(e.toString());
        }
    }

    /**
     * Constructs a SpaceMarine with the specified attributes, including health and loyalty.
     *
     * @param name the name of the SpaceMarine
     * @param coordinates the coordinates of the SpaceMarine
     * @param health the health of the SpaceMarine
     * @param loyal the loyalty status of the SpaceMarine
     * @param achievements the achievements of the SpaceMarine
     * @param meleeWeapon the melee weapon of the SpaceMarine
     * @param chapter the chapter of the SpaceMarine
     */
    public SpaceMarine(String name, Coordinates coordinates, Double health, Boolean loyal, String achievements,
            MeleeWeapon meleeWeapon, Chapter chapter) {
        this(name, coordinates, achievements, meleeWeapon, chapter);
        try {
            if (health != null && health > 0 && (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))) {
                this.health = health;
                this.loyal = loyal;
                // this.id = IDgenerator.getNextId();
            } else
                throw new IllegalArgumentException("Health must be above 0");
        } catch (Exception e) {
            console.writeln(e.toString());
        }
    }

    /**
     * Returns a string representation of the SpaceMarine object.
     *
     * @return a string description of the SpaceMarine
     */
    @Override
    public String toString() {
        String description = this.getClass().toString() + ": " + name.toString();
        description += "\n  id: " + id.toString();
        description += "\n  coordinates: " + coordinates.toString();
        description += "\n  creationDate: " + creationDate.toString();
        description += "\n  achievements: " + achievements;
        description += "\n  meleeWeapon: " + meleeWeapon.toString();
        description += "\n  chapter: " + chapter.toString();
        description += "\n  health: " + (health == null ? "null" : health);
        description += "\n  loyal: " + (loyal == null ? "null" : loyal);

        return description;
    }

    /**
     * Returns the hash code for the SpaceMarine object.
     *
     * @return the hash code
     */

    public int hashCode() {
        int result = 1;
        result = 31 * result + ((name == null) ? 0 : name.hashCode());
        result = 31 * result + ((coordinates == null) ? 0 : coordinates.hashCode());
        result = 31 * result + ((creationDate == null) ? 0 : creationDate.hashCode());
        result = 31 * result + ((health == null) ? 0 : health.hashCode());
        result = 31 * result + (loyal == null ? -1 : (loyal ? 1231 : 1237));
        result = 31 * result + ((achievements == null) ? 0 : achievements.hashCode());
        result = 31 * result + ((meleeWeapon == null) ? 0 : meleeWeapon.hashCode());
        result = 31 * result + ((chapter == null) ? 0 : chapter.hashCode());
        return result;
    }

    /**
     * Checks if this SpaceMarine is equal to another object.
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
        SpaceMarine other = (SpaceMarine) obj;
        if (id == null) {
            return other.id == null;
        } else {
            return id.equals(other.id);
        }
    }

    /**
     * Compares this SpaceMarine to another SpaceMarine based on their names.
     *
     * @param other the other SpaceMarine to compare
     * @return a negative integer, zero, or a positive integer as this SpaceMarine
     *         is less than, equal to, or greater than the specified SpaceMarine
     */
    @Override
    public int compareTo(SpaceMarine other) {
        return this.name.compareTo(other.name);
    }
}
