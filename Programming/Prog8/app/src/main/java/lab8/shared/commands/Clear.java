package lab8.shared.commands;

import java.util.Stack;
import java.util.stream.Collectors;

import lab8.shared.collection.CollectionManager;
import lab8.shared.model.SpaceMarine;

/**
 * Command to clear the collection managed by the CollectionManager.
 */
public class Clear extends Command {
    private Long id;
    /**
     * Executes the command to clear the collection.
     */
    @Override
    public void execute() {
        if (id==null)
            CollectionManager.getInstance().setCollection((CollectionManager.getInstance().getCollection().stream()
                    .filter(m -> !m.getCreatorId().equals(userId)).collect(Collectors.toCollection(Stack::new))));
        else
            CollectionManager.getInstance().setCollection((CollectionManager.getInstance().getCollection().stream()
                    .filter(m -> !(m.getCreatorId().equals(userId) && m.getId().equals(id))).collect(Collectors.toCollection(Stack::new))));
    }

    public <T> Command setArgs(T obj) {
        if (obj instanceof Long) {
            id = (Long) obj;
        }
        return this;
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
