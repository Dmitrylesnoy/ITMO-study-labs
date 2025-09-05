package lab7.shared.commands;

import lab7.shared.builders.SpaceMarineGenerator;

/**
 * The Add class extends the Command interface and is responsible for adding
 * a SpaceMarine object to a collection. It can create a new SpaceMarine using
 * a builder with using user params.
 */
public class AddRandom extends Command {
    private int cnt = 1;

    /**
     * Default constructor.
     */
    public AddRandom() {
    }

    /**
     * Constructor with a SpaceMarine object.
     *
     * @param addMarine SpaceMarine to add
     */
    public AddRandom(int cnt) {
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
            addRand.setUser(userId);
            addRand.execute();
        }
    }

    public <T> Command setArgs(T cnt) {
        this.cnt = (int) cnt;
        return this;
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
        return "Add random";
    }
}
