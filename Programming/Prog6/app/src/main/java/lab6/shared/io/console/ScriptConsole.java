package lab6.shared.io.console;

import java.util.LinkedList;
import java.util.Queue;

/**
 * ScriptConsole is a specialized console that extends StdConsole.
 * Script console need to parse line by line command from script and put those into Handler processing
 * It provides functionality for reading and writing data using a queue .
 * This class implements methods to manage console input and output.
 */
public class ScriptConsole extends StdConsole {
    private static Queue<String> data = new LinkedList<String>();
    private static ScriptConsole instance;

    /**
     * Default constructor for the ScriptConsole class, initializing an instance.
     */
    public ScriptConsole() {
        this.instance = this;
    }

    /**
     * Reads a line of input from the queue.
     *
     * @return the next line of input, or null if the queue is empty
     */
    public static String read() {
        return data.poll();
    }
    
    /*
     * Writes a line of output to the queue.
     *
     * @param line the line to write
     */
    // public static void write(String line) {
        // data.add(line);
    // }
    
    /*
     * Writes a line of output to the queue followed by a newline.
     *
     * @param line the line to write
     */
    // public static void writeln(String line) {
        // data.add(line);
    // }

    public void add(String line) {
        // ScriptConsole.getInstance().add(line);
        data.add(line);
    }

    /**
     * Retrieves the singleton instance of the ScriptConsole.
     *
     * @return the instance of ScriptConsole
     */
    public static ScriptConsole getInstance() {
        return instance != null ? instance : new ScriptConsole();
    }
}
