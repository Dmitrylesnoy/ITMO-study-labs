package lab5.commands;

import lab5.utils.CollectionManager;

public class Clear implements Command {
    private CollectionManager cm;
    
    public Clear(CollectionManager cm) {
        this.cm = cm;
    }

    @Override
    public void execute() {
        cm.getCollection().clear();
    }

    @Override
    public String describe() {
        return "This command clears the Collection";
    }

}
