package lab5.spacemarines;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import lab5.utils.MeleeWeaponAdapter;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder = { "id", "name", "coordinates", "creationDate", "health", "loyal", "achievements", "meleeWeapon","chapter" })
public class SpaceMarine {
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
    @XmlJavaTypeAdapter(MeleeWeaponAdapter.class)
    private MeleeWeapon meleeWeapon; // Поле может быть null
    private Chapter chapter; // Поле может быть null
    
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
                this.id = (long) hashCode();
            } else
                throw new IllegalArgumentException("Argument mustn't be null");
        } catch (Exception e) {
            System.out.println(e.toString());
        }
    }

    public SpaceMarine(String name, Coordinates coordinates, double health, boolean loyal, String achievements,
            MeleeWeapon meleeWeapon, Chapter chapter) {
        this(name, coordinates, achievements, meleeWeapon, chapter);
        try {
            if (health > 0 && health > 0) {
                this.health = health;
                this.loyal = loyal;
                this.id = (long) hashCode();
            } else
                throw new IllegalArgumentException("Health must be above 0");
        } catch (Exception e) {
            System.out.println(e.toString());
        }
    }

    @Override
    public String toString() {
        String desctiption = this.getClass().toString() + ": " + name.toString();
        desctiption += "\n  " + id.toString();
        desctiption += "\n  " + coordinates.toString();
        desctiption += "\n  " + creationDate.toString();
        desctiption += "\n  " + achievements;
        desctiption += "\n  " + meleeWeapon.toString();
        desctiption += "\n  " + chapter.toString();
        desctiption += "\n  " + health;
        desctiption += "\n  " + loyal;

        return desctiption;
    }

    // @Override
    // public int hashCode() {
    //     return hashCode(true);
    // }

    // public int hashCode(boolean abs) {
    //     int result = 1;
    //     result = 31 * result + ((id == null) ? 0 : id.hashCode());
    //     result = 31 * result + ((name == null) ? 0 : name.hashCode());
    //     result = 31 * result + ((coordinates == null) ? 0 : coordinates.hashCode());
    //     result = 31 * result + ((creationDate == null) ? 0 : creationDate.hashCode());
    //     long temp;
    //     temp = Double.doubleToLongBits(health);
    //     result = 31 * result + (int) (temp ^ (temp >>> 32));
    //     result = 31 * result + (loyal ? 1231 : 1237);
    //     result = 31 * result + ((achievements == null) ? 0 : achievements.hashCode());
    //     result = 31 * result + ((meleeWeapon == null) ? 0 : meleeWeapon.hashCode());
    //     result = 31 * result + ((chapter == null) ? 0 : chapter.hashCode());
    //     if (abs == false) {
    //         return result;
    //     } else {
    //         return result > 0 ? result : -result;
    //     }
    // }

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

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Coordinates getCoordinates() {
        return coordinates;
    }

    public void setCoordinates(Coordinates coordinates) {
        this.coordinates = coordinates;
    }

    public java.util.Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(java.util.Date creationDate) {
        this.creationDate = creationDate;
    }

    public double getHealth() {
        return health;
    }

    public void setHealth(double health) {
        this.health = health;
    }

    public boolean isLoyal() {
        return loyal;
    }

    public void setLoyal(boolean loyal) {
        this.loyal = loyal;
    }

    public String getAchievements() {
        return achievements;
    }

    public void setAchievements(String achievements) {
        this.achievements = achievements;
    }

    public MeleeWeapon getMeleeWeapon() {
        return meleeWeapon;
    }

    public void setMeleeWeapon(MeleeWeapon meleeWeapon) {
        this.meleeWeapon = meleeWeapon;
    }

    public Chapter getChapter() {
        return chapter;
    }

    public void setChapter(Chapter chapter) {
        this.chapter = chapter;
    }
}
