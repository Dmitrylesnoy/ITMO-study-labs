package lab5.system.commands;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

/**
 * Command to print SpaceMarine objects with unique loyalty values.
 * This class implements the Command interface and provides functionality
 * to identify and display SpaceMarines that have a unique loyalty status.
 */
public class PrintUniqueLoyal implements Command {
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the PrintUniqueLoyal class, initializing an instance without specific parameters.
     */
    public PrintUniqueLoyal() {
    }

    /**
     * Executes the command to find and print SpaceMarine objects with unique loyalty values.
     * It counts the occurrences of loyalty values and appends the unique ones to the output.
     */
    @Override
    public void execute() {
        Stack<SpaceMarine> collection = CollectionManager.getInstance().getCollection();
        Map<Boolean, Integer> loyalCount = new HashMap<>();

        for (SpaceMarine marine : collection) {
            Boolean loyal = marine.getLoyal();
            if (loyal != null)
                loyalCount.put(loyal, loyalCount.getOrDefault(loyal, 0) + 1);
        }

        for (SpaceMarine marine : collection) {
            if (loyalCount.get(marine.getLoyal()) != null && loyalCount.get(marine.getLoyal()) == 1) {
                output.append(marine.toString()).append("\n");
            }
        }
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
}
