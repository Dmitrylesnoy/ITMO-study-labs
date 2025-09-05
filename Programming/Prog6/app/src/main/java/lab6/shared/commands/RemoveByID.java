package lab6.shared.commands;

import java.util.List;
import java.util.Stack;
import java.util.Collection;
import java.util.stream.Collectors;

import lab6.shared.collection.CollectionManager;
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
    public  Command setArgs(Long id) {
        this.id = (Long)id;
        return this;
    }

    /**
     * Executes the command to remove the SpaceMarine with the specified ID from the
     * collection.
     */
    @Override
    public void execute() {

        // Stack<SpaceMarine> newStack = new Stack<SpaceMarine>();
        // newStack.addAll(CollectionManager.getInstance().getCollection().stream()
        //         .filter(m -> m.getId() != id)
        //         .collect(Collectors.toList()));
        StdConsole.writeln("Given id "+id+" " +id.getClass());
        Stack<SpaceMarine> newStack = CollectionManager.getInstance().getCollection().stream()
                .filter(mar -> mar.getId().equals(id)==false).collect(Collectors.toCollection(Stack::new));
        StdConsole.writeln(newStack.toString());
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
