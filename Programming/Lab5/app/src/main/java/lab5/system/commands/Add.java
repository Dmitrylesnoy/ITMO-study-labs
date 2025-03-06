package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class Add implements Command {
    private SpaceMarine obj;
    private CollectionManager cm;

    public Add(CollectionManager cm, SpaceMarine obj) {
        this.cm = cm;
        this.obj = obj;
    }

    @Override
    public void execute() {
        cm.Add(obj);
    }

    @Override
    public String describe() {
        return "This command Add concrete Object into collection";
    }
}
