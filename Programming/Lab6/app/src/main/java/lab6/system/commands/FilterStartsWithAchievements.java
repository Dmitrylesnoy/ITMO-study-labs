package lab6.system.commands;

import java.util.stream.Stream;

import lab6.server.Collection.CollectionManager;
import lab6.system.model.SpaceMarine;

/**
 * Command to filter SpaceMarine objects based on whether their achievements
 * start with a specified substring. This class implements the Command interface
 * and provides functionality to execute the filtering operation.
 */
public class FilterStartsWithAchievements implements Command {
    private String sub;
    private StringBuilder output = new StringBuilder();

    /**
     * Default constructor for the FilterStartsWithAchievements class, initializing an instance without a specific substring.
     */
    public FilterStartsWithAchievements() {
    }

    /**
     * Constructor that initializes the substring for filtering.
     *
     * @param sub the substring to filter achievements by
     */
    public FilterStartsWithAchievements(String sub) {
        this.sub = sub;
    }

    /**
     * Sets the substring for filtering achievements.
     *
     * @param sub the substring to filter achievements by
     */
    public void setArgs(String sub) {
        this.sub = sub;
    }

    /**
     * Executes the filtering operation, appending all SpaceMarine objects
     * whose achievements start with the specified substring to the output.
     */
    @Override
    public void execute() {
        CollectionManager.getInstance().getCollection().stream()
                .filter(m -> m.getAchievements().startsWith(sub))
            .forEach(m -> output.append(m.toString()).append("\n"));
    }

    /**
     * Retrieves the output of the filtering operation.
     *
     * @return a string containing the filtered SpaceMarine objects
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
        return "Return all elements, which fields Achievements start with custom string";
    }
}
