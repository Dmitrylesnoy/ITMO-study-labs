package lab5.system.commands;

import lab5.system.commands.Command;
import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class FilterStartsWithAchievements implements Command {
    private CollectionManager cm;
    private String sub;
    private StringBuilder output=new StringBuilder();

    public FilterStartsWithAchievements(CollectionManager cm, String sub) {
        this.cm = cm;
        this.sub = sub;
    }

    @Override
    public void execute() {
        for (SpaceMarine marine : cm.getCollection()) {
            if (marine.getAchievements().startsWith(sub)) {
                output.append(marine.toString()).append("\n");
            }
        }
    }

    public String getOutput() {
        return output.toString();
    }

    @Override
    public String describe() {
        return "Return all elements, which fields Achivements starts with custom string";
    }

}
