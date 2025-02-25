package lab5.commands;

import lab5.spacemarines.SpaceMarine;
import lab5.utils.CollectionManager;

public class RemoveByID implements Command{
    private CollectionManager cm;
    private Long id;

    public RemoveByID(CollectionManager cm, Long id) {
        this.cm = cm;
        this.id = id;
    }
    @Override
    public void execute() {
        for (SpaceMarine marine : cm.getCollection()) {
            if (marine.getId() == id) {
                cm.getCollection().remove(marine);
            }
        }
    }

    @Override
    public String describe() {
        return "This command removes an element from the collection by its id";
    }

}
