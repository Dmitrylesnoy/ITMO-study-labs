package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

import java.util.Stack;

public class MinByMeleeWeapon implements Command {
    private CollectionManager cm;
    private StringBuilder output=new StringBuilder();

    public MinByMeleeWeapon(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> collection = cm.getCollection();
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

    @Override
    public String describe() {
        return "Returns any object with the minimum MeleeWeapon value";
    }
}
