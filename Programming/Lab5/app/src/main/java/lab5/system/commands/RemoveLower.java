package lab5.system.commands;

import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

public class RemoveLower implements Command {
    private SpaceMarine dobj;

    public RemoveLower(){}
    public RemoveLower(SpaceMarine dobj) {
        this.dobj = dobj;
    }

    @Override
    public void execute() {
        if (dobj == null) {
            dobj = new SpaceMarineBuilder().build();
        }
        Stack<SpaceMarine> col = new Stack<SpaceMarine>();
        col.addAll(CollectionManager.getInstance().getCollection());
        for (SpaceMarine marine : col) {
            if (marine.getId() < dobj.getId()) {
                new RemoveByID(marine.getId()).execute();
            }
        }
    }

    public String describe() {
        return "Delete all elements from the collection that are lower than a given one";
    }

}
