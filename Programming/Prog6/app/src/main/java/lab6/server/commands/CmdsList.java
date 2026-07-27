package lab6.server.commands;

import java.util.HashMap;
import java.util.Map;

/**
 * Command to provide a list of available commands along with their
 * descriptions.
 * This class implements the Command interface and allows users to see what
 * commands
 * are available in the system.
 */
public class CmdsList implements Command {
    private StringBuilder output = new StringBuilder();
    private static Map<String, Command> alwCmds = new HashMap<String, Command>();

    static {

        alwCmds.put("add", new Add());
        alwCmds.put("add_random", new AddRandom());
        alwCmds.put("clear", new Clear());
        alwCmds.put("cmds_list", new CmdsList());
        alwCmds.put("exit", new Exit());
        alwCmds.put("load", new Load());
        // alwCmds.put("save", new Save());
        alwCmds.put("info", new Info());
        alwCmds.put("show", new Show());
        alwCmds.put("sort", new Sort());
        alwCmds.put("filter_starts_with_achievements", new FilterStartsWithAchievements());
        alwCmds.put("min_by_meleeweapon", new MinByMeleeWeapon());
        alwCmds.put("remove_by_id", new RemoveByID());
        alwCmds.put("remove_greater", new RemoveGreater());
        alwCmds.put("remove_lower", new RemoveLower());
        alwCmds.put("update_id", new UpdateId());
        alwCmds.put("print_unique_loyal", new PrintUniqueLoyal());
        alwCmds.put("execute_script", null);
        alwCmds.put("help", new Help());
    }

    public CmdsList() {
    }

    public CmdsList(Map<String, Command> cmds) {
        CmdsList.alwCmds = cmds;
    }

    public void execute() {
        alwCmds.keySet().stream().forEach(name -> output
                .append(name + " \n"));
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return list of all allowed commands";
    }

    public String getName() {
        return "Commands list";
    }

    public static Map<String, Command> getAllowedCmds() {
        return alwCmds;
    }
}
