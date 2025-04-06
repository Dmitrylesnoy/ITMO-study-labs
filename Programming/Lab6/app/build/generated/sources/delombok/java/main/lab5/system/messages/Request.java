package lab5.system.messages;

/**
 * Represents a command request containing a command name and its associated arguments.
 * This class provides constructors for initializing the command and its arguments,
 * and uses Lombok annotations to generate getter and setter methods.
 */
public class Request {
    private String command;
    private String[] args;

    /**
     * Constructs a Request with the specified command name.
     *
     * @param name the name of the command
     */
    public Request(String name) {
        this.command = name.toLowerCase();
    }

    /**
     * Constructs a Request with the specified command name and arguments.
     *
     * @param command the name of the command
     * @param args the arguments associated with the command
     */
    public Request(String command, String[] args) {
        this.command = command.toLowerCase();
        this.args = args;
    }

    public String getCommand() {
        return this.command;
    }

    public String[] getArgs() {
        return this.args;
    }

    public void setCommand(final String command) {
        this.command = command;
    }

    public void setArgs(final String[] args) {
        this.args = args;
    }
}
