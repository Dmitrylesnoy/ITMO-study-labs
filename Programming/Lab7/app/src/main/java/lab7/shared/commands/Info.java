package lab7.shared.commands;

import lab7.shared.collection.CollectionManager;

public class Info implements Command {
    private StringBuilder output = new StringBuilder();

    public Info() {
    }

    public void execute() {
        output.append(CollectionManager.getInstance().getTime() + "\n");
        output.append("Collection lenght: " + CollectionManager.getInstance().getCollection().size());
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return date of creating collection and it's lenght";
    }
    
    public String getName() {
        return "Info";
    }
}
