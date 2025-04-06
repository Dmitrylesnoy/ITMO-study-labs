package lab5.system.commands;

import lab5.system.utils.CollectionManager;

/**
 * Command to clear the collection managed by the CollectionManager.
 */
public class Clear implements Command {

    /**
     * Executes the command to clear the collection.
     */
    @Override
    public void execute() {
        CollectionManager.getInstance().getCollection().clear();
    }

    /**
     * Describes the command's functionality.
     *
     * @return description of the command
     */
    public String describe() {
        return "This command clears the Collection";
    }
}
