package lab8.shared.commands;

import java.util.Comparator;
import java.util.Stack;

import lab8.shared.collection.CollectionManager;
import lab8.shared.model.SpaceMarine;

/**
 * Command to find the SpaceMarine object with the minimum melee weapon value.
 * This class extends the Command interface and provides functionality to
 * retrieve the SpaceMarine with the least powerful melee weapon.
 */
public class MinByMeleeWeapon extends Command {
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the MinByMeleeWeapon class, initializing an instance
     * without specific parameters.
     */
    public MinByMeleeWeapon() {
    }

    /**
     * Executes the command to find the SpaceMarine with the minimum melee weapon
     * value.
     * If the collection is empty, it appends a message indicating that the
     * collection is empty.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> collection = CollectionManager.getInstance().getCollection();
        if (collection.isEmpty()) {
            output.append("Collection is empty").append("\n");
            return;
        }
        output.append("SpaceMarine with the minimum melee weapon is: ")
                .append((collection.stream().min(Comparator.comparing(m -> m.getMeleeWeapon().ordinal())).get())
                        .getName())
                .append("\n");
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

    public String getName() {
        return "Min by MeleeWeapon";
    }
}
