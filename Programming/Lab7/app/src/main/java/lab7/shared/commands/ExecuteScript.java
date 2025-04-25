package lab7.shared.commands;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import lab7.client.Handler;
import lab7.shared.io.console.StdConsole;

/**
 * The ExecuteScript class implements the Command interface and is responsible for executing
 * a script file that contains a series of commands. Each command in the script is processed
 * sequentially, allowing for batch execution of commands.
 */
public class ExecuteScript implements Command {
    private String scriptFilePath;

    /**
     * Default constructor for the ExecuteScript class.
     */
    public ExecuteScript() {
    }

    /**
     * Constructor that initializes the script file path.
     *
     * @param scriptFilePath the path to the script file to be executed
     */
    public ExecuteScript(String scriptFilePath) {
        this.scriptFilePath = scriptFilePath;
    }

    public <T> Command setArgs(T scriptFilePath) {
        this.scriptFilePath = (String)scriptFilePath;
        return this;
    }
    /**
     * Executes the script by reading commands from the specified script file.
     * Each line in the script file is treated as a command and is processed
     * using the Handler class.
     *
     * @throws IOException if an I/O error occurs during reading the script file
     *                     or if the script file is not found
     */
    @Override
    public void execute() throws IOException {
        try {
            StdConsole console = StdConsole.getInstance();
            File scriptFile = new File(scriptFilePath);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(scriptFile));
            bufferedReader.lines().peek(l -> console.writeln("added  " + l))
                .forEach(l -> console.add(l));
            bufferedReader.close();
        } catch (FileNotFoundException e) {
            throw new FileNotFoundException("Script file not found: " + scriptFilePath);
        }
        throw new UnsupportedOperationException();
    }

    /**
     * Provides a description of the command's functionality.
     *
     * @return a string describing the purpose of this command, which is to execute
     *         a script containing program commands
     */
    public String describe() {
        return "Execute script with program commands";
    }
    
    public String getName() {
        return "Execute script";
    }
}
