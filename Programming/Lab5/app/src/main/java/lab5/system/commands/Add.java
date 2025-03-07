package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

public class Add implements Command {
    private SpaceMarine obj;

    public Add(){}

    public Add(SpaceMarine addMarine) {
        this.obj = addMarine;
    }

    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        CollectionManager.getInstance().Add(obj);
    }

    public String describe() {
        return "This command Add concrete Object into collection";
    }
}
