package lab5.system.commands;

import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class FilterStartsWithAchievements implements Command {
    private String sub;
    private StringBuilder output = new StringBuilder();

    public FilterStartsWithAchievements() {
    }

    public FilterStartsWithAchievements(String sub) {
        this.sub = sub;
    }

    public void setArgs(String sub) {
        this.sub = sub;
    }

    @Override
    public void execute() {
        for (SpaceMarine marine : CollectionManager.getInstance().getCollection()) {
            if (marine.getAchievements().startsWith(sub)) {
                output.append(marine.toString()).append("\n");
            }
        }
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return all elements, which fields Achivements starts with custom string";
    }

}
