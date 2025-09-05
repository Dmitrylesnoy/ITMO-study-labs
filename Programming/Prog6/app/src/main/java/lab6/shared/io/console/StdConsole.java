package lab6.shared.io.console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.LinkedList;
import java.util.Queue;

/**
 * StdConsole provides standard input and output functionality for console applications.
 * This class allows reading from the console and writing output to it, using buffered
 * readers and writers for efficient I/O operations.
 */
public class StdConsole {
    private static final BufferedReader consoleReader = new BufferedReader(new InputStreamReader(System.in));
    private static final BufferedWriter consoleWriter = new BufferedWriter(new OutputStreamWriter(System.out));
    private  Queue<String> data = new LinkedList<String>();

    private static StdConsole getinstance;

    /**
     * Default constructor for the StdConsole class, initializing an getinstance.
     */
    public StdConsole() {
        this.getinstance = this;
    }

    /**
     * Reads a line of input from the console.
     *
     * @return the input line, or null if an I/O error occurs
     */
    public static String read() {
        try {
            if (StdConsole.getInstance().isEmpty() == false)
                return StdConsole.getInstance().poll();
            return consoleReader.readLine();
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Reads a line of input from the console after displaying a prompt.
     *
     * @param prompt the prompt message to display
     * @return the input line, or null if an I/O error occurs
     */
    public static String read(String prompt) {
        if (StdConsole.getInstance().isEmpty() == false)
            return StdConsole.getInstance().poll();
        writeln(prompt);
        return read();
    }

    /**
     * Writes a line of text to the console.
     *
     * @param text the text to write
     */
    public static void write(String text) {
        try {
            consoleWriter.append(text).flush();
        } catch (IOException e) {
            // Handle exception
        }
    }

    /**
     * Writes a line of text to the console followed by a newline.
     *
     * @param text the text to write
     */
    public static void writeln(String text) {
        try {
            consoleWriter.append(text).append(System.lineSeparator()).flush();
        } catch (IOException e) {
            // Handle exception
        }
    }

    public void add(String line) {
        data.add(line);
    }

    public String poll() {
        if (isEmpty() == false)
            return data.poll();
        else
            return "";
    }

    public boolean isEmpty() {
        return data.isEmpty();
    }
    /**
     * Closes the console reader and writer.
     *
     * @throws Exception if an I/O error occurs
     */
    public void close() throws Exception {
        consoleReader.close();
        consoleWriter.close();
    }

    /**
     * Retrieves the singleton getinstance of the StdConsole.
     *
     * @return the getinstance of StdConsole
     */
    public static StdConsole getInstance() {
        return getinstance != null ? getinstance : new StdConsole();
    }
}
