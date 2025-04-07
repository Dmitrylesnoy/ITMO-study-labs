package lab6.system.commands;

import lab6.system.collection.CollectionManager;

/**
 * Command to load a collection from a file. This class implements the Command interface
 * and provides functionality to retrieve the collection data from persistent storage.
 */
public class Load implements Command {
    /**
     * Default constructor for the Load class, initializing an instance without specific parameters.
     */
    public Load() {
    }

    /**
     * Executes the load command, retrieving the collection from a file using the CollectionManager.
     */
    @Override
    public void execute() {
        CollectionManager.getInstance().load();
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command loads the collection from a file";
    }
    
    public String getName() {
        return "Load";
    }
}
