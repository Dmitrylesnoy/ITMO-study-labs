package lab5.commands;

import lab5.utils.CollectionManager;
import lab5.spacemarines.SpaceMarine;
import java.util.Stack;

public class MinByMeleeWeapon implements Command {
    private CollectionManager cm;

    public MinByMeleeWeapon(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> collection = cm.getCollection();
        if (collection.isEmpty()) {
            System.out.println("Collection is empty.");
            return;
        }

        SpaceMarine minMarine = collection.peek();
        for (SpaceMarine marine : collection) {
            if (marine.getMeleeWeapon().ordinal() < minMarine.getMeleeWeapon().ordinal()) {
                minMarine = marine;
            }
        }
        System.out.println("SpaceMarine with minimum melee weapon: " + minMarine);
    }

    @Override
    public String describe() {
        return "Returns any object with the minimum MeleeWeapon value";
    }
}
