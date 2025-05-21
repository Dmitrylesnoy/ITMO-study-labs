package lab8.shared.commands;

import lab8.shared.collection.CollectionManager;
import lab8.shared.model.SpaceMarine;

/**
 * Command to display all SpaceMarine objects in the collection.
 * This class extends the Command interface and provides functionality
 * to retrieve and present the details of all SpaceMarines.
 */
public class LoadClient extends Command {

    /**
     * Default constructor for the Show class, initializing an instance without
     * specific parameters.
     */
    public LoadClient() {
    }

    /**
     * Executes the command to retrieve and display all SpaceMarine objects in the
     * collection.
     */
    public void execute() {

    }

    /**
     * Describes the command's functionality.
     *.
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Return list of all elements in Collection";
    }

    public String getName() {
        return "Show";
    }
}
