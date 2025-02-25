package lab5.commands;

import java.util.Stack;

import lab5.utils.CollectionManager;

public class Show implements Command {
    private CollectionManager cm;

    public Show(CollectionManager cm) {
        this.cm = cm;
    }

    public void execute() {
        for (Object i : cm.getCollection()) {
            System.out.println(i.toString());
        }
    }
    
    public String describe() {
        return "Return list of all elements in Collection";
    }
}
