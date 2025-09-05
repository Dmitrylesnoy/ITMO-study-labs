package lab6.shared.commands;

import java.util.Collection;
import java.util.Stack;

import lab6.shared.collection.CollectionManager;
import lab6.shared.collection.IDgenerator;
import lab6.shared.model.SpaceMarine;

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
    public Add() {
    }

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
        // if (obj == null) {
        //     obj = new SpaceMarineBuilder().build();
        // }
        if (obj.getId()==null) obj.setId(CollectionManager.getInstance().nextId());
        CollectionManager.getInstance().getCollection().add(obj);
    }

    /**
     * Describes the command's functionality.
     *
     * @return description of the command
     */
    public String describe() {
        return "Adds a SpaceMarine to the collection.";
    }

    public String getName() {
        return "Add";
    }

    public <T> Command setArgs(T spacemarine){
        obj = (SpaceMarine)spacemarine;
        return this;
    }
}
