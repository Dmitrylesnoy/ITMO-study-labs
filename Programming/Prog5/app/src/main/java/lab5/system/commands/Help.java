package lab5.system.commands;

import java.util.Map;


/**
 * Command to provide a list of available commands along with their descriptions.
 * This class implements the Command interface and allows users to see what commands
 * are available in the system.
 */
public class Help implements Command {
    private StringBuilder output = new StringBuilder();
    private Map<String, Command> helpCmds;

    /**
     * Default constructor for the Help class, initializing an instance without specific commands.
     */
    public Help() {
    }

    /**
     * Constructor that initializes the Help command with a map of available commands.
     *
     * @param cmds a map of command names to their corresponding Command objects
     */
    public Help(Map<String, Command> cmds) {
        this.helpCmds = cmds;
    }

    /**
     * Executes the help command, appending the list of available commands and their descriptions
     * to the output.
     */
    public void execute() {
        for (String name : helpCmds.keySet()) {
            output.append(" - " + name + "  " + helpCmds.get(name).describe() + "\n");
        }
    }

    /**
     * Retrieves the output of the help command.
     *
     * @return a string containing the list of available commands and their descriptions
     */
    public String getOutput() {
        return output.toString();
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Return list of all commands with description";
    }
}
