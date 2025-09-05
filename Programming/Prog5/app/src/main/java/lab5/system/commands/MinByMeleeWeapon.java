package lab5.system.commands;

import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

/**
 * Command to find the SpaceMarine object with the minimum melee weapon value.
 * This class implements the Command interface and provides functionality to
 * retrieve the SpaceMarine with the least powerful melee weapon.
 */
public class MinByMeleeWeapon implements Command {
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the MinByMeleeWeapon class, initializing an instance without specific parameters.
     */
    public MinByMeleeWeapon() {
    }

    /**
     * Executes the command to find the SpaceMarine with the minimum melee weapon value.
     * If the collection is empty, it appends a message indicating that the collection is empty.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> collection = CollectionManager.getInstance().getCollection();
        if (collection.isEmpty()) {
            output.append("Collection is empty").append("\n");
            return;
        }

        SpaceMarine minMarine = collection.peek();
        for (SpaceMarine marine : collection) {
            if (marine.getMeleeWeapon().ordinal() < minMarine.getMeleeWeapon().ordinal()) {
                minMarine = marine;
            }
        }
        output.append("SpaceMarine with the minimum melee weapon is: ").append(minMarine.getName()).append("\n");
    }

    /**
     * Retrieves the output of the command execution.
     *
     * @return a string containing the result of the command
     */
    public String getOutput() {
        return output.toString();
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Returns any object with the minimum MeleeWeapon value";
    }
}
