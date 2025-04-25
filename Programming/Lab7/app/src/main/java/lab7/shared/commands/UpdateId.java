package lab7.shared.commands;

import java.util.Stack;

import lab7.shared.collection.CollectionManager;
import lab7.shared.model.SpaceMarine;

/**
 * Command to update a SpaceMarine object in the collection based on its ID.
 * This class extends the Command interface and provides functionality to
 * replace an existing SpaceMarine with a new one that has the same ID.
 */
public class UpdateId extends Command {
    private SpaceMarine obj;

    /**
     * Default constructor for the UpdateId class, initializing an instance without
     * specific parameters.
     */
    public UpdateId() {
    }

    /**
     * Constructor that initializes the UpdateId command with a specific SpaceMarine
     * object.
     *
     * @param obj the SpaceMarine object to update in the collection
     */
    public UpdateId(SpaceMarine obj) {
        this.obj = obj;
    }

    /**
     * Executes the command to update the SpaceMarine object in the collection.
     * If no object is specified, a new SpaceMarine is created using the
     * SpaceMarineBuilder.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> updateStack = CollectionManager.getInstance().getCollection();
        updateStack.stream()
                .filter(marine -> marine.getId() == obj.getId())
                .forEach(marine -> marine = obj);
        CollectionManager.getInstance().setCollection(updateStack);
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command updates object in Collection, which ID equal to given object ID.";
    }

    public String getName() {
        return "Update ID";
    }

    public <T> Command setArgs(T marine) {
        obj = (SpaceMarine) marine;
        return this;
    }
}
