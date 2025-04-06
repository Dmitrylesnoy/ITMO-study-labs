package lab5.system.commands;

import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

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
        Stack<SpaceMarine> col = new Stack<SpaceMarine>();
        col.addAll(CollectionManager.getInstance().getCollection());
        for (SpaceMarine marine : col) {
            if (marine.getId() > obj.getId()) {
                new RemoveByID(marine.getId()).execute();
            }
        }
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
