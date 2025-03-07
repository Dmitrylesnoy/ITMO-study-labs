package lab5.system.commands;

import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

public class RemoveGreater implements Command {
    private SpaceMarine obj;

    public RemoveGreater(){}
    public RemoveGreater(SpaceMarine obj) {
        this.obj = obj;
    }

    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        Stack<SpaceMarine> col = new Stack<SpaceMarine>();
        col.addAll(CollectionManager.getInstance().getCollection());
        for (SpaceMarine marine : col) {
            if (marine.getId() > obj.getId()) {
                new RemoveByID(marine.getId()).execute();
            }
        }
    }

    public String describe() {
        return "Delete all elements from the collection that are greater than a given one";
    }

}
