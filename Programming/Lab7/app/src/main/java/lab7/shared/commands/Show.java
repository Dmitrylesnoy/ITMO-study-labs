package lab7.shared.commands;

import lab7.shared.collection.CollectionManager;

/**
 * Command to display all SpaceMarine objects in the collection.
 * This class extends the Command interface and provides functionality
 * to retrieve and present the details of all SpaceMarines.
 */
public class Show extends Command {
    private StringBuilder output = new StringBuilder(" ");

    /**
     * Default constructor for the Show class, initializing an instance without
     * specific parameters.
     */
    public Show() {
    }

    /**
     * Executes the command to retrieve and display all SpaceMarine objects in the
     * collection.
     */
    public void execute() {
        CollectionManager.getInstance().getCollection().stream()
                .map(m -> m.toString()).forEach(m -> output.append(m).append('\n'));
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

    public String getName() {
        return "Show";
    }
}
