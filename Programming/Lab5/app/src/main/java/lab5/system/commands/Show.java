package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class Show implements Command {
    private StringBuilder output = new StringBuilder();

    public Show() {
    }

    public void execute() {
        for (SpaceMarine i : CollectionManager.getInstance().getCollection()) {
            output.append(i.toString() + "\n");
        }
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return list of all elements in Collection";
    }
}
