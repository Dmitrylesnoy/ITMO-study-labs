package lab5.system.commands;

import lab5.system.utils.CollectionManager;

public class Save implements Command {
    public Save() {
    }

    @Override
    public void execute() {
        CollectionManager.getInstance().save();
    }

    public String describe() {
        return "This command saves the collection to a file";
    }

}
