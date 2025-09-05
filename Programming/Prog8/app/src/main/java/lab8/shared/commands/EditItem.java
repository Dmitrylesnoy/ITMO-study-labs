package lab8.shared.commands;

import java.util.Collection;
import java.util.Stack;
import java.util.stream.Collectors;

import lab8.shared.collection.CollectionManager;
import lab8.shared.model.SpaceMarine;

/**
 * Command to display all SpaceMarine objects in the collection.
 * This class extends the Command interface and provides functionality
 * to retrieve and present the details of all SpaceMarines.
 */
public class EditItem extends Command {
    private SpaceMarine edited;

    /**
     * Default constructor for the Show class, initializing an instance without
     * specific parameters.
     */
    public EditItem() {
    }

    /**
     * Executes the command to retrieve and display all SpaceMarine objects in the
     * collection.
     */
    public void execute() {
        if (edited != null && edited.getCreatorId().equals(userId)) {
            CollectionManager.getInstance().setCollection(
                CollectionManager.getInstance().getCollection().stream()
                    .map(mar -> mar.getId().equals(edited.getId()) ? edited : mar)
                    .collect(Collectors.toCollection(Stack<SpaceMarine>::new))
            );
        }
    }

    public <T> Command setArgs(T obj) {
        if (obj instanceof SpaceMarine) {
            edited = (SpaceMarine) obj;
        }
        return this;
    }

    /**
     * Describes the command's functionality.
     *.
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Create a part of copy Collection from server";
    }

    public String getName() {
        return "LoadPart";
    }
}
