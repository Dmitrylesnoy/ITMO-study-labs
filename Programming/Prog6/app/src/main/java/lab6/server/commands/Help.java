package lab6.server.commands;

import java.util.Map;

/**
 * Command to provide a list of available commands along with their
 * descriptions.
 * This class implements the Command interface and allows users to see what
 * commands
 * are available in the system.
 */
public class Help implements Command {
    private StringBuilder output = new StringBuilder();
    private Map<String, Command> helpCmds;

    /**
     * Default constructor for the Help class, initializing an instance without
     * specific commands.
     */
    public Help() {
        helpCmds = CmdsList.getAllowedCmds();
    }

    /**
     * Constructor that initializes the Help command with a map of available
     * commands.
     *
     * @param cmds a map of command names to their corresponding Command objects
     */
    public Help(Map<String, Command> cmds) {
        this.helpCmds = cmds;
    }

    /**
     * Executes the help command, appending the list of available commands and their
     * descriptions
     * to the output.
     */
    public void execute() {
        output = new StringBuilder();
        if (helpCmds == null) {
            return;
        }

        helpCmds.forEach((name, command) -> {
            String description = command == null ? "not available" : command.describe();
            output.append(" - ").append(name).append("  ").append(description).append("\n");
        });
    }

    /**
     * Retrieves the output of the help command.
     *
     * @return a string containing the list of available commands and their
     *         descriptions
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

    public String getName() {
        return "Help";
    }
}
