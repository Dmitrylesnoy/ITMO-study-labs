package lab5.commands;

import java.util.Map;
import java.util.Stack;

import lab5.utils.CollectionManager;
import lab5.spacemarines.SpaceMarine;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class PrintUniqueLoyal implements Command {
    private CollectionManager cm;

    public PrintUniqueLoyal(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> collection = cm.getCollection();
        Map<Boolean, Integer> loyalCount = new HashMap<>();

        for (SpaceMarine marine : collection) {
            boolean loyal = marine.isLoyal();
            loyalCount.put(loyal, loyalCount.getOrDefault(loyal, 0) + 1);
        }

        for (SpaceMarine marine : collection) {
            if (loyalCount.get(marine.isLoyal()) == 1) {
                System.out.println(marine);
            }
        }
    }

    @Override
    public String describe() {
        return "Prints objects with unique Loyal fields for all elements";
    }
}
