package lab5.system.io.console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

/**
 * StdConsole provides standard input and output functionality for console applications.
 * This class allows reading from the console and writing output to it, using buffered
 * readers and writers for efficient I/O operations.
 */
public class StdConsole {
    private static final BufferedReader consoleReader = new BufferedReader(new InputStreamReader(System.in));
    private static final BufferedWriter consoleWriter = new BufferedWriter(new OutputStreamWriter(System.out));
    private static StdConsole instance;

    /**
     * Default constructor for the StdConsole class, initializing an instance.
     */
    public StdConsole() {
        this.instance = this;
    }

    /**
     * Reads a line of input from the console.
     *
     * @return the input line, or null if an I/O error occurs
     */
    public static String read() {
        try {
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
     * Retrieves the singleton instance of the StdConsole.
     *
     * @return the instance of StdConsole
     */
    public static StdConsole getInstance() {
        return instance != null ? instance : new StdConsole();
    }
}
