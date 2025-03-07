package lab5.system.commands;

import java.util.Map;

import lab5.system.utils.CollectionManager;

public class Help implements Command {
    private StringBuilder output = new StringBuilder();
    private Map<String, Command> helpCmds;

    public Help() {
    }

    public Help(Map<String, Command> cmds) {
        this.helpCmds = cmds;
    }

    public void execute() {
        for (String name : helpCmds.keySet()) {
            output.append(" - " + name + "  " + helpCmds.get(name).describe() + "\n");
        }
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return list of all command with description";
    }
}
