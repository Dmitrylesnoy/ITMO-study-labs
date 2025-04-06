package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

/**
 * Command to remove a SpaceMarine object from the collection by its ID.
 * This class implements the Command interface and provides functionality
 * to delete a specific SpaceMarine based on the provided ID.
 */
public class RemoveByID implements Command {
    private Long id;

    /**
     * Default constructor for the RemoveByID class, initializing an instance without specific parameters.
     */
    public RemoveByID() {
    }

    /**
     * Constructor that initializes the RemoveByID command with a specific ID.
     *
     * @param id the ID of the SpaceMarine to be removed
     */
    public RemoveByID(Long id) {
        this.id = id;
    }

    /**
     * Sets the ID of the SpaceMarine to be removed.
     *
     * @param id the ID of the SpaceMarine to be removed
     */
    public void setArgs(Long id) {
        this.id = id;
    }

    /**
     * Executes the command to remove the SpaceMarine with the specified ID from the collection.
     */
    @Override
    public void execute() {
        for (SpaceMarine marine : CollectionManager.getInstance().getCollection()) {
            if (marine.getId() == id) {
                CollectionManager.getInstance().getCollection().remove(marine);
                break;
            }
        }
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command removes an element from the collection by its id";
    }
}
