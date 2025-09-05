package lab8.shared.commands;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.stream.Collectors;

import lab8.shared.collection.CollectionManager;
import lab8.shared.model.SpaceMarine;

/**
 * Command to display all SpaceMarine objects in the collection.
 * This class extends the Command interface and provides functionality
 * to retrieve and present the details of all SpaceMarines.
 */
public class LoadPart extends Command {
    private Integer start = 0;
    private int len = 10;
    private Deque<SpaceMarine> part = new ArrayDeque<SpaceMarine>();

    /**
     * Default constructor for the Show class, initializing an instance without
     * specific parameters.
     */
    public LoadPart() {
    }

    /**
     * Executes the command to retrieve and display all SpaceMarine objects in the
     * collection.
     */
    public void execute() {
        int end = start + len;
        if (start + len >= CollectionManager.getInstance().getCollection().size()) 
            end = CollectionManager.getInstance().getCollection().size();
        
        part.clear();
        part.addAll(CollectionManager.getInstance().getCollection()
                    .stream().skip(start)
                            .limit(end-start+1)
                            .collect(Collectors.toList()));
    }

    public <T> Command setArgs(T obj) {
        if (obj instanceof Integer) {
            start = (Integer) obj;
        }
        return this;
    }

    @Override
    public String getOutput() {
        if (start + len >= CollectionManager.getInstance().getCollection().size())
            return "FULL";
        return String.valueOf(len + start);
    }

    public Deque<SpaceMarine> getPart() {
        return part;
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
