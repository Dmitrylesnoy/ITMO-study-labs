package lab5.system.commands;

import java.util.Collection;
import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class RemoveLower implements Command {
    private SpaceMarine curMarine;
    private CollectionManager cm;

    public RemoveLower(CollectionManager cm, SpaceMarine curMarine) {
        this.curMarine = curMarine;
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> col = cm.getCollection();
        for (SpaceMarine marine : col) {
            if (marine.getId() < curMarine.getId()) {
                new RemoveByID(cm, marine.getId()).execute();
           }
        }
    }

    @Override
    public String describe() {
        return "Delete all elements from the collection that are lower than a given one";
    }

}
