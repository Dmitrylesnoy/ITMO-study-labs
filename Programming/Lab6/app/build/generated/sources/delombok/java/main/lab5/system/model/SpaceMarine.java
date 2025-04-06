package lab5.system.model;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lab5.system.io.console.StdConsole;

/**
 * Represents a SpaceMarine entity with various attributes such as id, name,
 * coordinates, health, loyalty, achievements, melee weapon, and chapter.
 * This class provides constructors for initializing these attributes and
 * methods for generating hash codes, equality checks, and string representation.
 */
@JacksonXmlRootElement(localName = "SpaceMarine")
public class SpaceMarine implements Comparable<SpaceMarine> {
    private Long id; // Field cannot be null, must be greater than 0, unique, and generated automatically
    private String name; // Field cannot be null, cannot be empty
    private Coordinates coordinates; // Field cannot be null
    private java.util.Date creationDate; // Field cannot be null, generated automatically
    private Double health; // Must be greater than 0
    private Boolean loyal;
    private String achievements; // Field cannot be null
    private MeleeWeapon meleeWeapon; // Field can be null
    private Chapter chapter; // Field can be null
    private StdConsole console = new StdConsole();

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
    public SpaceMarine(String name, Coordinates coordinates, String achievements, MeleeWeapon meleeWeapon, Chapter chapter) {
        try {
            if (name != null && name == "") {
                throw new IllegalAccessError("Name mustn\'t be empty String");
            } else if (name != null && coordinates != null && achievements != null && meleeWeapon != null && chapter != null) {
                this.name = name;
                this.coordinates = coordinates;
                this.creationDate = new java.util.Date();
                this.achievements = achievements;
                this.meleeWeapon = meleeWeapon;
                this.chapter = chapter;
                this.id = (long) hashCode(false);
            } else throw new IllegalArgumentException("Argument mustn\'t be null");
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
    public SpaceMarine(String name, Coordinates coordinates, Double health, Boolean loyal, String achievements, MeleeWeapon meleeWeapon, Chapter chapter) {
        this(name, coordinates, achievements, meleeWeapon, chapter);
        try {
            if (health != null && health > 0 && (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))) {
                this.health = health;
                this.loyal = loyal;
                this.id = (long) hashCode(false);
            } else throw new IllegalArgumentException("Health must be above 0");
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
    @Override
    public int hashCode() {
        return hashCode(true);
    }

    /**
     * Returns the hash code for the SpaceMarine object, with an option for absolute value.
     *
     * @param abs whether to return the absolute value of the hash code
     * @return the hash code
     */
    public int hashCode(boolean abs) {
        int result = 1;
        result = 31 * result + ((name == null) ? 0 : name.hashCode());
        result = 31 * result + ((coordinates == null) ? 0 : coordinates.hashCode());
        result = 31 * result + ((creationDate == null) ? 0 : creationDate.hashCode());
        result = 31 * result + ((health == null) ? 0 : health.hashCode());
        result = 31 * result + (loyal == null ? -1 : (loyal ? 1231 : 1237));
        result = 31 * result + ((achievements == null) ? 0 : achievements.hashCode());
        result = 31 * result + ((meleeWeapon == null) ? 0 : meleeWeapon.hashCode());
        result = 31 * result + ((chapter == null) ? 0 : chapter.hashCode());
        // return Objects.hash(name, coordinates, creationDate, health, loyal, achievements, meleeWeapon, chapter);
        return abs ? (result > 0 ? result : -result) : result;
    }

    /**
     * Checks if this SpaceMarine is equal to another object.
     *
     * @param obj the object to compare
     * @return true if equal, false otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
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

    public Long getId() {
        return this.id;
    }

    public String getName() {
        return this.name;
    }

    public Coordinates getCoordinates() {
        return this.coordinates;
    }

    public java.util.Date getCreationDate() {
        return this.creationDate;
    }

    public Double getHealth() {
        return this.health;
    }

    public Boolean getLoyal() {
        return this.loyal;
    }

    public String getAchievements() {
        return this.achievements;
    }

    public MeleeWeapon getMeleeWeapon() {
        return this.meleeWeapon;
    }

    public Chapter getChapter() {
        return this.chapter;
    }

    public StdConsole getConsole() {
        return this.console;
    }

    public void setId(final Long id) {
        this.id = id;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public void setCoordinates(final Coordinates coordinates) {
        this.coordinates = coordinates;
    }

    public void setCreationDate(final java.util.Date creationDate) {
        this.creationDate = creationDate;
    }

    public void setHealth(final Double health) {
        this.health = health;
    }

    public void setLoyal(final Boolean loyal) {
        this.loyal = loyal;
    }

    public void setAchievements(final String achievements) {
        this.achievements = achievements;
    }

    public void setMeleeWeapon(final MeleeWeapon meleeWeapon) {
        this.meleeWeapon = meleeWeapon;
    }

    public void setChapter(final Chapter chapter) {
        this.chapter = chapter;
    }

    public void setConsole(final StdConsole console) {
        this.console = console;
    }
}
