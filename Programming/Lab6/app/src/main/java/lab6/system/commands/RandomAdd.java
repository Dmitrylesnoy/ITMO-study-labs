package lab6.system.commands;

import lab6.system.model.SpaceMarineGenerator;

/**
 * The Add class implements the Command interface and is responsible for adding
 * a SpaceMarine object to a collection. It can create a new SpaceMarine using
 * a builder with using user params.
 */
public class RandomAdd implements Command {
    private int cnt = 1;

    /**
     * Default constructor.
     */
    public RandomAdd() {
    }

    /**
     * Constructor with a SpaceMarine object.
     *
     * @param addMarine SpaceMarine to add
     */
    public RandomAdd(int cnt) {
        this.cnt = cnt;
    }

    /**
     * Executes the command to add the SpaceMarine.
     * Creates a new SpaceMarine if none is provided.
     */
    @Override
    public void execute() {
        for (int i = 0; i < cnt; i++) {
            Add addRand = new Add(new SpaceMarineGenerator().generate());
            addRand.execute();
        }
    }

    public void setArgs(int cnt) {
        this.cnt = cnt;
    }

    /**
     * Describes the command's functionality.
     *
     * @return description of the command
     */
    public String describe() {
        return "Adds a SpaceMarine to the collection.";
    }

    @Override
    public String getName() {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'getName'");
    }
}
