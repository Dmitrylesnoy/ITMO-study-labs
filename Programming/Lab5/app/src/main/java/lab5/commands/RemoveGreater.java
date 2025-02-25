package lab5.commands;

import java.util.Collection;
import java.util.Stack;

import lab5.spacemarines.SpaceMarine;
import lab5.utils.CollectionManager;

public class RemoveGreater implements Command {
    private SpaceMarine curMarine;
    private CollectionManager cm;

    public RemoveGreater(CollectionManager cm, SpaceMarine curMarine) {
        this.curMarine = curMarine;
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> col = cm.getCollection();
        for (SpaceMarine marine : col) {
            if (marine.getId() > curMarine.getId()) {
                new RemoveByID(cm, marine.getId()).execute();
           }
        }
    }

    @Override
    public String describe() {
        return "Delete all elements from the collection that are greater than a given one";
    }

}
