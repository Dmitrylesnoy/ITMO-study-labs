package lab5.system.io.Console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

/**
 * Class which defines console which user will use to communicate with program.
 */
public class StdConsole {
    private static final BufferedReader consoleReader = new BufferedReader(new InputStreamReader(System.in));
    private static final BufferedWriter consoleWriter = new BufferedWriter(new OutputStreamWriter(System.out));

    public static String read(String prompt) {
        writeln(prompt);
        return read();
    }

    public static String read() {
        try {
            return consoleReader.readLine();
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Prompts to console without placing \n at the end of prompt.
     * 
     * @param text to prompt to user
     */
    public static void write(String text) {
        try {
            consoleWriter.append(text).flush();
        } catch (IOException e) {
        }
    }

    public static void writeln(String text) {
        try {
            consoleWriter.append(text).append(System.lineSeparator()).flush();
        } catch (IOException e) {
        }
    }

    public static void close() throws Exception {
        consoleReader.close();
        consoleWriter.close();
    }
}
