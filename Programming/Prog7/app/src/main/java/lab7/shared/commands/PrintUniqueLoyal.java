package lab7.shared.commands;

import java.util.Set;
import java.util.stream.Collectors;

import lab7.shared.collection.CollectionManager;

/**
 * Command to print SpaceMarine objects with unique loyalty values.
 * This class extends the Command interface and provides functionality
 * to identify and display SpaceMarines that have a unique loyalty status.
 */
public class PrintUniqueLoyal extends Command {
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the PrintUniqueLoyal class, initializing an instance
     * without specific parameters.
     */
    public PrintUniqueLoyal() {
    }

    /**
     * Executes the command to find and print SpaceMarine objects with unique
     * loyalty values.
     * It counts the occurrences of loyalty values and appends the unique ones to
     * the output.
     */
    @Override
    public void execute() {
        Set<Boolean> loyalList = CollectionManager.getInstance().getCollection().stream().map(m -> m.getLoyal())
                .distinct().collect(Collectors.toSet());
        output.append(loyalList.toString()).append("\n");
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
        return "Prints objects with unique Loyal fields for all elements";
    }

    public String getName() {
        return "Print unique loyal";
    }
}
