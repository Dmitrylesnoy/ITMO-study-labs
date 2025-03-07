package lab5.system.commands;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class Sort implements Command {

    public Sort() {
    }

    @Override
    public void execute() {
        List<SpaceMarine> list = new ArrayList<>(CollectionManager.getInstance().getCollection());
        Collections.sort(list);
        new Clear().execute();
        CollectionManager.getInstance().getCollection().addAll(list);
    }

    public String describe() {
        return "Sorts the stack of SpaceMarine objects in natural order.";
    }
}
