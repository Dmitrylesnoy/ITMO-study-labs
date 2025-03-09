package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

/**
 * The Add class implements the Command interface and is responsible for adding
 * a SpaceMarine object to a collection. It can create a new SpaceMarine using
 * a builder with using user params.
 */
public class Add implements Command {
    private SpaceMarine obj;

    /**
     * Default constructor.
     */
    public Add(){}

    /**
     * Constructor with a SpaceMarine object.
     *
     * @param addMarine SpaceMarine to add
     */
    public Add(SpaceMarine addMarine) {
        this.obj = addMarine;
    }

    /**
     * Executes the command to add the SpaceMarine.
     * Creates a new SpaceMarine if none is provided.
     */
    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        CollectionManager.getInstance().Add(obj);
    }

    /**
     * Describes the command's functionality.
     *
     * @return description of the command
     */
    public String describe() {
        return "Adds a SpaceMarine to the collection.";
    }
}
