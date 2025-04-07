package lab6.system.commands;

import java.io.IOException;

/**
 * The Command interface defines the structure for command classes in the application.
 * It requires implementing classes to provide an execution method and a description.
 */
public interface Command {
    /**
     * Executes the command.
     *
     * @throws IOException if an I/O error occurs during execution
     */
    public void execute() throws IOException;

    /**
     * Provides a description of the command's functionality.
     *
     * @return a string describing the command
     */
    public String describe();

    // public void setArgs(String[] args);

    /**
     * Returns the output of the command.
     *
     * @return the output as a string
     */
    public default String getOutput() {
        return "";
    }

    public String getName();
}
