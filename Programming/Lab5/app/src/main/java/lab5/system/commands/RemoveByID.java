package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class RemoveByID implements Command {
    private Long id;

    public RemoveByID() {
    }

    public RemoveByID(Long id) {
        this.id = id;
    }

    public void setArgs(Long id) {
        this.id = id;
    }

    @Override
    public void execute() {
        for (SpaceMarine marine : CollectionManager.getInstance().getCollection()) {
            if (marine.getId() == id) {
                CollectionManager.getInstance().getCollection().remove(marine);
                break;
            }
        }
    }

    public String describe() {
        return "This command removes an element from the collection by its id";
    }

}
