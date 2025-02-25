package lab5.commands;

import lab5.commands.Command;
import lab5.spacemarines.SpaceMarine;
import lab5.utils.CollectionManager;

public class FilterStartsWithAchievements implements Command {
    private CollectionManager cm;
    private String sub;

    public FilterStartsWithAchievements(CollectionManager cm, String sub) {
        this.cm = cm;
        this.sub = sub;
    }

    @Override
    public void execute() {
        for (SpaceMarine marine : cm.getCollection()) {
            if (marine.getAchievements().startsWith(sub)) {
                System.out.println(marine.toString());
            }
        }
    }

    @Override
    public String describe() {
        return "Return all elements, which fields Achivements starts with custom string";
    }

}
