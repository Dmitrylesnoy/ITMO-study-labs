package lab7.shared.commands;

import java.io.IOException;

/**
 * The Command interface defines the structure for command classes in the application.
 * It requires implementing classes to provide an execution method and a description.
 */
import java.io.Serializable;

public abstract class Command implements Serializable {
    protected Integer userId;
    /**
     * Executes the command.
     *
     * @throws IOException if an I/O error occurs during execution
     */
    public abstract void execute() throws IOException;

    /**
     * Provides a description of the command's functionality.
     *
     * @return a string describing the command
     */
    public abstract String describe();

    public void setUser(Integer userId) {
        this.userId=userId;
    }

    // public void setArgs(String[] args);

    /**
     * Returns the output of the command.
     *
     * @return the output as a string
     */
    public String getOutput() {
        return "";
    }

    public abstract String getName();

    public <T> Command setArgs(T obj){
        return this;
    }
}
