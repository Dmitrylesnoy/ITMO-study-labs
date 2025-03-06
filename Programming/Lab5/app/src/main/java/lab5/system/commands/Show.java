package lab5.system.commands;

import lab5.system.utils.CollectionManager;

public class Show implements Command {
    private CollectionManager cm;
    private StringBuilder output = new StringBuilder();

    public Show(CollectionManager cm) {
        this.cm = cm;
    }

    public void execute() {
        for (Object i : cm.getCollection()) {
            output.append(i.toString()).append("\n");
        }
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return list of all elements in Collection";
    }
}
