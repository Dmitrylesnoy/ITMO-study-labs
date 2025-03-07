package lab5.system.commands;

import lab5.system.utils.CollectionManager;

public class Load implements Command {
    public Load() {
    }

    @Override
    public void execute() {
        CollectionManager.getInstance().load();
    }

    public String describe() {
        return "This command loads the collection from a file";
    }

}
