package lab7.shared.commands;

import java.util.Stack;
import java.util.stream.Collectors;

import lab7.shared.builders.SpaceMarineBuilder;
import lab7.shared.collection.CollectionManager;
import lab7.shared.model.SpaceMarine;

/**
 * Command to remove all SpaceMarine objects from the collection that are lower than a specified object.
 * This class implements the Command interface and provides functionality to delete SpaceMarines
 * based on their ID comparison.
 */
public class RemoveLower implements Command {
    private SpaceMarine obj;

    /**
     * Default constructor for the RemoveLower class, initializing an instance without specific parameters.
     */
    public RemoveLower() {}

    /**
     * Constructor that initializes the RemoveLower command with a specific SpaceMarine object.
     *
     * @param obj the SpaceMarine object to compare against
     */
    public RemoveLower(SpaceMarine obj) {
        this.obj = obj;
    }

    /**
     * Executes the command to remove SpaceMarine objects with IDs lower than the specified object's ID.
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
                        .filter(mar -> mar.compareTo(obj) < 0)
                        .collect(Collectors.toList()));
        CollectionManager.getInstance().setCollection(newStack);
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Delete all elements from the collection that are lower than a given one";
    }
    
    public String getName() {
        return "Remove lower";
    }
    
    public <T> Command setArgs(SpaceMarine marine) {
        obj = (SpaceMarine)marine;
        return this;
    }
}
