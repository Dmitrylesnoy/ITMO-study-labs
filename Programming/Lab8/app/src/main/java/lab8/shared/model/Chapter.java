package lab8.shared.model;

import java.io.Serializable;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lab8.shared.exceptions.NullArgumetnException;
import lab8.shared.io.console.StdConsole;
import lombok.Getter;
import lombok.Setter;

/**
 * Represents a chapter entity with attributes for its name and world.
 * This class provides methods for setting and getting the values of these
 * attributes,
 * as well as methods for generating hash codes and equality checks.
 */
@Getter
@Setter
@JacksonXmlRootElement(localName = "Chapter")
public class Chapter implements Serializable {
    @JacksonXmlElementWrapper(localName = "name")
    private String name; // Field cannot be null, cannot be empty
    @JacksonXmlElementWrapper(localName = "world")
    private String world; // Field can be null

    /**
     * Default constructor for the Chapter class.
     */
    // private Chapter() {
    // }

    /**
     * Constructs a Chapter with the specified name and world.
     *
     * @param name  the name of the chapter
     * @param world the world of the chapter
     */
    public Chapter(String name, String world) {
        try {
            if (name == "")
                throw new IllegalArgumentException("Name mustn't be Empty String");
            else {
                this.name = name;
                this.world = world;
            }
        } catch (Exception e) {
            new StdConsole();
            StdConsole.writeln(e.toString());
        }
    }

    /**
     * Returns the name of the chapter.
     *
     * @return the name of the chapter
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the chapter.
     *
     * @param name the new name of the chapter
     */
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

    /**
     * Returns the world of the chapter.
     *
     * @return the world of the chapter
     */
    public String getWorld() {
        return world;
    }

    /**
     * Sets the world of the chapter.
     *
     * @param world the new world of the chapter
     */
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

    /**
     * Returns the hash code for the Chapter object.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        int result = 1;
        result = 31 * result + ((name == null) ? 0 : name.hashCode());
        result = 31 * result + ((world == null) ? 0 : world.hashCode());
        return result;
    }

    /**
     * Checks if this Chapter object is equal to another object.
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

    /**
     * Returns a string representation of the Chapter object.
     *
     * @return a string description of the Chapter
     */
    @Override
    public String toString() {
        return this.getClass().toString() + ", " + name + ", " + world;
    }
}
