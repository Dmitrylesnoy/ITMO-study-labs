package lab5.spacemarines;

@XmlRootElement
@XmlType(propOrder = { "id", "name", "coordinates", "creationDate", "health", "loyal", "achievements", "meleeWeapon", "chapter" })
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
    private MeleeWeapon meleeWeapon; // Поле может быть null
    private Chapter chapter; // Поле может быть null
    

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

    @Override
    public int hashCode() {
        int hash = name.hashCode();
        hash = hash * 31 + coordinates.hashCode();
        hash = hash * 31 + creationDate.hashCode();
        hash = hash * 31 + achievements.hashCode();
        hash = hash * 31 + meleeWeapon.hashCode();
        hash = hash * 31 + chapter.hashCode();
        hash = hash * 31 + (int)health;
        hash = hash * 31 + (loyal==true ? 1 : 0);
        return (hash>0? hash : -hash);
    }
}
