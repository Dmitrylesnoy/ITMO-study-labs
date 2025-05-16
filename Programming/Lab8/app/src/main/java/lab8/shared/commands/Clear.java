package lab8.shared.commands;

import java.util.Stack;
import java.util.stream.Collectors;

import lab8.shared.collection.CollectionManager;

/**
 * Command to clear the collection managed by the CollectionManager.
 */
public class Clear extends Command {

    /**
     * Executes the command to clear the collection.
     */
    @Override
    public void execute() {
        CollectionManager.getInstance().setCollection((CollectionManager.getInstance().getCollection().stream()
                .filter(m -> !m.getCreator_id().equals(userId)).collect(Collectors.toCollection(Stack::new))));
    }

    /**
     * Describes the command's functionality.
     *
     * @return description of the command
     */
    public String describe() {
        return "This command clears the Collection";
    }

    public String getName() {
        return "Clear";
    }
}
