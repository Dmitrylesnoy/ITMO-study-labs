package lab8.shared.commands;

/**
 * Command to terminate the program. This class extends the Command interface
 * and provides functionality to exit the application without saving the current
 * state.
 */
public class Exit extends Command {

    /**
     * Executes the exit command, terminating the program.
     */
    @Override
    public void execute() {
        // new Save().execute();
        // System.exit(0);
    }

    /**
     * Describes the command's functionality.
     *
     * @return a string describing the purpose of this command
     */
    public String describe() {
        return "Break down the program process without saving collection in file";
    }

    public String getName() {
        return "Exit";
    }
}
