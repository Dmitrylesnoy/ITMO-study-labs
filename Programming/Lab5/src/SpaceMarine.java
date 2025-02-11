public class SpaceMarine {
    private Long id; // Поле не может быть null, Значение поля должно быть больше 0, Значение этого
                     // поля должно быть уникальным, Значение этого поля должно генерироваться автоматически
    private String name; // Поле не может быть null, Строка не может быть пустой
    private Coordinates coordinates; // Поле не может быть null
    private java.util.Date creationDate; // Поле не может быть null, Значение этого поля должно генерироваться автоматически
    private double health; // Значение поля должно быть больше 0
    private boolean loyal;
    private String achievements; // Поле не может быть null
    private MeleeWeapon meleeWeapon; // Поле может быть null
    private Chapter chapter; // Поле может быть null
}
