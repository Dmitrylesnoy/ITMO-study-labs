package lab5.spacemarines;

// @XmlRootElement
public class Chapter {
    private String name; // Поле не может быть null, Строка не может быть пустой
    private String world; // Поле может быть null

    public Chapter(String name, String world) {
        try {
            if (name == "")
                throw new IllegalArgumentException("Name mustn't be Empty String");
            else {
                this.name = name;
                this.world = world;
            }
        } catch (Exception e) {
            System.out.println(e.toString());
        }
    }
    
    @Override
    public String toString() {
        return this.getClass().toString() + ", " + name + ", " + world;
    }
}
