package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

public class UpdateId implements Command {
    private SpaceMarine obj;

    public UpdateId(){}
    public UpdateId(SpaceMarine obj) {
        this.obj = obj;
    }

    @Override
    public void execute() {
        if (obj == null) {
            obj = new SpaceMarineBuilder().build();
        }
        for (SpaceMarine marine : CollectionManager.getInstance().getCollection()) {
            if (marine.getId() == obj.getId()) {
                marine = obj;
            }
        }
    }

    public String describe() {
        return "This command updates object in Collection, which ID equal to given object ID.";
    }

}
