package lab5.commands;

import lab5.spacemarines.SpaceMarine;
import lab5.utils.CollectionManager;

public class UpdateId implements Command{
    private SpaceMarine obj;
    private CollectionManager cm;

    
    public UpdateId(CollectionManager cm, SpaceMarine obj) {
        this.cm = cm;
        this.obj = obj;
    }

    @Override
    public void execute() {
        for (SpaceMarine marine : cm.getCollection()) {
            if (marine.getId() == obj.getId()) {
                marine = obj;
            }
        }
    }

    @Override
    public String describe() {
        return "This command updates object in Collection, which ID equal to given object ID.";
    }
    
}
