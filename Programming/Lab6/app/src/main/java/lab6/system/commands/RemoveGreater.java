package lab6.system.commands;

import java.util.Stack;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import lab6.server.Collection.CollectionManager;
import lab6.system.model.SpaceMarine;
import lab6.system.model.builders.SpaceMarineBuilder;

/**
 * Command to remove all SpaceMarine objects from the collection that are greater than a specified object.
 * This class implements the Command interface and provides functionality to delete SpaceMarines
 * based on their ID comparison.
 */
public class RemoveGreater implements Command {
    private SpaceMarine obj;

    /**
     * Default constructor for the RemoveGreater class, initializing an instance without specific parameters.
     */
    public RemoveGreater() {}

    /**
     * Constructor that initializes the RemoveGreater command with a specific SpaceMarine object.
     *
     * @param obj the SpaceMarine object to compare against
     */
    public RemoveGreater(SpaceMarine obj) {
        this.obj = obj;
    }

    /**
     * Executes the command to remove SpaceMarine objects with IDs greater than the specified object's ID.
     * If no object is specified, a new SpaceMarine is created using the SpaceMarineBuilder.
     */
    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        Stack<SpaceMarine> newStack = new Stack<SpaceMarine>();
        newStack.addAll(
                CollectionManager.getInstance().getCollection().stream()
                        .filter(mar -> mar.compareTo(obj) > 0)
                        .collect(Collectors.toList()));
        CollectionManager.getInstance().setCollection(newStack);

    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Delete all elements from the collection that are greater than a given one";
    }
}
