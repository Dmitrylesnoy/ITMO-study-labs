package lab7.shared.commands;

import java.util.Stack;
import java.util.stream.Collectors;

import lab7.shared.collection.CollectionManager;
import lab7.shared.model.SpaceMarine;

/**
 * Command to sort SpaceMarine objects in the collection.
 * This class extends the Command interface and provides functionality
 * to sort the collection of SpaceMarines in their natural order.
 */
public class Sort extends Command {

    /**
     * Default constructor for the Sort class, initializing an instance without
     * specific parameters.
     */
    public Sort() {
    }

    /**
     * Executes the command to sort the SpaceMarine objects in the collection.
     * It creates a list from the collection, sorts it, clears the original
     * collection,
     * and adds the sorted list back to the collection.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> newStack = new Stack<SpaceMarine>();
        newStack.addAll(CollectionManager.getInstance().getCollection().stream()
                .filter(m -> m.getCreator_id().equals(userId))
                .sorted().collect(Collectors.toList()));
        CollectionManager.getInstance().setCollection(newStack);
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Sorts the stack of SpaceMarine objects in natural order.";
    }

    public String getName() {
        return "Sort";
    }
}
