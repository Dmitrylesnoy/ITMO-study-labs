package lab8.shared.builders;

import lab8.shared.io.console.ClientConsole;
import lab8.shared.model.Chapter;

/**
 * ChapterBuilder is responsible for constructing Chapter objects.
 * This class provides methods for setting the chapter's name and world,
 * ensuring that the name is not null or empty, while the world can be null.
 */
public class ChapterBuilder {
    private String name; // Field cannot be null, String cannot be empty
    private String world; // Field can be null
    private ClientConsole console;

    /**
     * Default constructor for the ChapterBuilder class, initializing the name and
     * world.
     */
    public ChapterBuilder() {
        setName();
        setWorld();
    }

    /**
     * Builds and returns a Chapter object with the specified name and world.
     *
     * @return a new Chapter object
     */
    public Chapter build() {
        return new Chapter(name, world);
    }

    /**
     * Prompts the user to enter the name of the chapter and validates the input.
     */
    public void setName() {
        while (true) {
            console.writeln("Enter the Name of the Chapter: ");
            String name = console.read();
            if (name != null && !name.isEmpty()) {
                this.name = name;
                break;
            } else {
                console.writeln("Invalid input: Name cannot be null or empty. Please try again.");
            }
        }
    }

    /**
     * Prompts the user to enter the world of the chapter and validates the input.
     */
    public void setWorld() {
        while (true) {
            String world = console.read("Enter the World of the Chapter: ");
            if (world != null && !world.isEmpty()) {
                this.world = world;
                break;
            } else {
                console.writeln("Invalid input: World cannot be null. Please try again.");
            }
        }
    }
}
