package lab5.system.commands;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class PrintUniqueLoyal implements Command {
    private CollectionManager cm;
    private StringBuilder output = new StringBuilder();

    public PrintUniqueLoyal(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        Stack<SpaceMarine> collection = cm.getCollection();
        Map<Boolean, Integer> loyalCount = new HashMap<>();

        for (SpaceMarine marine : collection) {
            Boolean loyal = marine.getLoyal();
            if (loyal != null)
                loyalCount.put(loyal, loyalCount.getOrDefault(loyal, 0) + 1);
        }

        for (SpaceMarine marine : collection) {
            if (loyalCount.get(marine.getLoyal()) == 1) {
                output.append(marine.toString()).append("\n");
            }
        }
    }

    public String getOutput() {
        return output.toString();
    }

    @Override
    public String describe() {
        return "Prints objects with unique Loyal fields for all elements";
    }
}
