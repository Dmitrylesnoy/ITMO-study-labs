package lab5.system.model;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lombok.Getter;
import lombok.Setter;

import lab5.system.io.console.StdConsole;
import lab5.system.io.console.*;

@Getter
@Setter
@JacksonXmlRootElement(localName = "SpaceMarine")
public class SpaceMarine implements Comparable<SpaceMarine> {
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
    private StdConsole console = new StdConsole();

    public SpaceMarine() {
    }

    public SpaceMarine(String name, Coordinates coordinates, String achievements, MeleeWeapon meleeWeapon,
            Chapter chapter) {
        try {
            if (name != null && name == "") {
                throw new IllegalAccessError("Name mustn't be empty String");
            } else if (name != null && coordinates != null && achievements != null && meleeWeapon != null
                    && chapter != null) {
                this.name = name;
                this.coordinates = coordinates;
                this.creationDate = new java.util.Date();
                this.achievements = achievements;
                this.meleeWeapon = meleeWeapon;
                this.chapter = chapter;
                this.id = (long) hashCode(false);
            } else
                throw new IllegalArgumentException("Argument mustn't be null");
        } catch (Exception e) {
            console.writeln(e.toString());
        }
    }

    public SpaceMarine(String name, Coordinates coordinates, Double health, Boolean loyal, String achievements,

            MeleeWeapon meleeWeapon, Chapter chapter) {
        this(name, coordinates, achievements, meleeWeapon, chapter);
        try {
            if (health != null && health > 0 && (Boolean.TRUE.equals(loyal) || Boolean.FALSE.equals(loyal))) {

                this.health = health;
                this.loyal = loyal;
                this.id = (long) hashCode(false);
            } else
                throw new IllegalArgumentException("Health must be above 0");
        } catch (Exception e) {
            console.writeln(e.toString());
        }
    }

    @Override
    public String toString() {
        String desctiption = this.getClass().toString() + ": " + name.toString();
        desctiption += "\n  id: " + id.toString();
        desctiption += "\n  coordinates: " + coordinates.toString();
        desctiption += "\n  creationDate: " + creationDate.toString();
        desctiption += "\n  achievements: " + achievements;
        desctiption += "\n  meleeWeapon: " + meleeWeapon.toString();
        desctiption += "\n  chapter: " + chapter.toString();
        desctiption += "\n  health: " + (health == null ? "null" : health);
        desctiption += "\n  loyal: " + (loyal == null ? "null" : loyal);

        return desctiption;
    }

    @Override
    public int hashCode() {
        return hashCode(true);
    }

    public int hashCode(boolean abs) {
        int result = 1;
        // result = 31 * result + ((id == null) ? 0 : id.hashCode());
        result = 31 * result + ((name == null) ? 0 : name.hashCode());
        result = 31 * result + ((coordinates == null) ? 0 : coordinates.hashCode());
        result = 31 * result + ((creationDate == null) ? 0 : creationDate.hashCode());
        result = 31 * result + ((health == null) ? 0 : health.hashCode());
        result = 31 * result + (loyal == null ? -1 : (loyal ? 1231 : 1237));
        result = 31 * result + ((achievements == null) ? 0 : achievements.hashCode());
        result = 31 * result + ((meleeWeapon == null) ? 0 : meleeWeapon.hashCode());
        result = 31 * result + ((chapter == null) ? 0 : chapter.hashCode());
        if (abs == false) {
            return result;
        } else {
            return result > 0 ? result : -result;
        }
    }

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
            if (other.id != null)
                return false;
        } else if (id.equals(other.id))
            return true;
        return false;
    }

    @Override
    public int compareTo(SpaceMarine other) {
        return this.name.compareTo(other.name);
    }
}
