package lab6.system.commands;

import lab6.server.Collection.CollectionManager;

/**
 * Command to save the collection of SpaceMarine objects to a file.
 * This class implements the Command interface and provides functionality
 * to persist the current state of the collection.
 */
public class Save implements Command {
    /**
     * Default constructor for the Save class, initializing an instance without specific parameters.
     */
    public Save() {
    }

    /**
     * Executes the command to save the collection to a file.
     * It calls the save method from the CollectionManager to perform the operation.
     */
    @Override
    public void execute() {
        CollectionManager.getInstance().save();
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command saves the collection to a file";
    }
}
