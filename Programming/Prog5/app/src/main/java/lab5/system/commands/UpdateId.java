package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

/**
 * Command to update a SpaceMarine object in the collection based on its ID.
 * This class implements the Command interface and provides functionality to
 * replace an existing SpaceMarine with a new one that has the same ID.
 */
public class UpdateId implements Command {
    private SpaceMarine obj;

    /**
     * Default constructor for the UpdateId class, initializing an instance without specific parameters.
     */
    public UpdateId() {}

    /**
     * Constructor that initializes the UpdateId command with a specific SpaceMarine object.
     *
     * @param obj the SpaceMarine object to update in the collection
     */
    public UpdateId(SpaceMarine obj) {
        this.obj = obj;
    }

    /**
     * Executes the command to update the SpaceMarine object in the collection.
     * If no object is specified, a new SpaceMarine is created using the SpaceMarineBuilder.
     */
    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        for (SpaceMarine marine : CollectionManager.getInstance().getCollection()) {
            if (marine.getId() == obj.getId()) {
                marine = obj;
            }
        }
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "This command updates object in Collection, which ID equal to given object ID.";
    }
}
