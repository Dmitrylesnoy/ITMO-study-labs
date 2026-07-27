package lab6.server.commands;

import java.util.Stack;
import java.util.stream.Collectors;

import lab6.server.collection.CollectionManager;
import lab6.shared.io.console.StdConsole;
import lab6.shared.model.SpaceMarine;

/**
 * Command to remove a SpaceMarine object from the collection by its ID.
 * This class implements the Command interface and provides functionality
 * to delete a specific SpaceMarine based on the provided ID.
 */
public class RemoveByID implements Command {
    private Long id;

    /**
     * Default constructor for the RemoveByID class, initializing an instance
     * without specific parameters.
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
    public <T> Command setArgs(T id) {
        this.id = (Long) id;
        StdConsole.writeln(id.toString());
        return this;
    }

    /**
     * Executes the command to remove the SpaceMarine with the specified ID from the
     * collection.
     */
    @Override
    public void execute() {
        if (id == null) {
            throw new IllegalArgumentException("ID must not be null");
        }

        Stack<SpaceMarine> newStack = CollectionManager.getInstance().getCollection().stream()
                .filter(mar -> mar != null && !id.equals(mar.getId()))
                .collect(Collectors.toCollection(Stack::new));

        CollectionManager.getInstance().setCollection(newStack);
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command removes an element from the collection by its id";
    }

    public String getName() {
        return "Remove by ID";
    }
}
