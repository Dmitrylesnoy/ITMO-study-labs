package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

/**
 * Command to display all SpaceMarine objects in the collection.
 * This class implements the Command interface and provides functionality
 * to retrieve and present the details of all SpaceMarines.
 */
public class Show implements Command {
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the Show class, initializing an instance without specific parameters.
     */
    public Show() {
    }

    /**
     * Executes the command to retrieve and display all SpaceMarine objects in the collection.
     */
    public void execute() {
        for (SpaceMarine i : CollectionManager.getInstance().getCollection()) {
            output.append(i.toString()).append("\n");
        }
    }

    /**
     * Retrieves the output of the command execution.
     *
     * @return a string containing the list of all SpaceMarine objects
     */
    public String getOutput() {
        return output.toString();
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Return list of all elements in Collection";
    }
}
