package lab5.system.commands;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class Sort implements Command {
    private CollectionManager cm;

    public Sort(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> stack = cm.getCollection();
        List<SpaceMarine> list = new ArrayList<>(stack);
        Collections.sort(list);
        cm.getCollection().clear();
        cm.getCollection().addAll(list);
    }

    @Override
    public String describe() {
        return "Sorts the stack of SpaceMarine objects in natural order.";
    }
}
