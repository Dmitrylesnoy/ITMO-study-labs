package lab5.system.commands;

import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class MinByMeleeWeapon implements Command {
    private StringBuilder output = new StringBuilder();

    public MinByMeleeWeapon() {
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> collection = CollectionManager.getInstance().getCollection();
        if (collection.isEmpty()) {
            output.append("Collection is empty").append("\n");
            return;
        }

        SpaceMarine minMarine = collection.peek();
        for (SpaceMarine marine : collection) {
            if (marine.getMeleeWeapon().ordinal() < minMarine.getMeleeWeapon().ordinal()) {
                minMarine = marine;
            }
        }
        output.append("SpaceMarine with the minimum melee weapon is: ").append(minMarine.getName()).append("\n");
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Returns any object with the minimum MeleeWeapon value";
    }
}
