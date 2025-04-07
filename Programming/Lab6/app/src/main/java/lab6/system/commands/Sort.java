package lab6.system.commands;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import lab6.server.Collection.CollectionManager;
import lab6.system.model.SpaceMarine;

/**
 * Command to sort SpaceMarine objects in the collection.
 * This class implements the Command interface and provides functionality
 * to sort the collection of SpaceMarines in their natural order.
 */
public class Sort implements Command {

    /**
     * Default constructor for the Sort class, initializing an instance without specific parameters.
     */
    public Sort() {
    }

    /**
     * Executes the command to sort the SpaceMarine objects in the collection.
     * It creates a list from the collection, sorts it, clears the original collection,
     * and adds the sorted list back to the collection.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> newStack = new Stack<SpaceMarine>();
        newStack.addAll(CollectionManager.getInstance().getCollection().stream()
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
}
