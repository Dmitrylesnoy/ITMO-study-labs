package lab5.system.commands;

import lab5.system.utils.CollectionManager;

public class Clear implements Command {

    @Override
    public void execute() {
        CollectionManager.getInstance().getCollection().clear();
    }

    public String describe() {
        return "This command clears the Collection";
    }

}
