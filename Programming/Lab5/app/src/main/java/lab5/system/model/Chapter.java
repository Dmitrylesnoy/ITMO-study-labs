package lab5.system.model;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lab5.system.exceptions.NullArgumetnException;
import lab5.system.io.Console.StdConsole;

@JacksonXmlRootElement(localName = "Chapter")
public class Chapter {
    @JacksonXmlElementWrapper(localName = "name")
    private String name; // Поле не может быть null, Строка не может быть пустой
    @JacksonXmlElementWrapper(localName = "world")
    private String world; // Поле может быть null

    private Chapter() {
    }

    public Chapter(String name, String world) {
        try {
            if (name == "")
                throw new IllegalArgumentException("Name mustn't be Empty String");
            else {
                this.name = name;
                this.world = world;
            }
        } catch (Exception e) {
            new StdConsole().writeln(e.toString());
        }
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        try {
            if (name != null && name != "") {
                this.name = name;
            } else
                throw new NullArgumetnException("Name mustn't be Empty String");
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
        }
    }

    public String getWorld() {
        return world;
    }

    public void setWorld(String world) {
        try {
            if (world != null) {
                this.world = world;
            } else
                throw new NullArgumetnException("");
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
        }
    }

    @Override
    public int hashCode() {
        int result = 1;
        result = 31 * result + ((name == null) ? 0 : name.hashCode());
        result = 31 * result + ((world == null) ? 0 : world.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Chapter other = (Chapter) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (world == null) {
            if (other.world != null)
                return false;
        } else if (!world.equals(other.world))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return this.getClass().toString() + ", " + name + ", " + world;
    }
}
