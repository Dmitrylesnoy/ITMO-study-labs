package lab5.system.io.console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class StdConsole {
    private static final BufferedReader consoleReader = new BufferedReader(new InputStreamReader(System.in));
    private static final BufferedWriter consoleWriter = new BufferedWriter(new OutputStreamWriter(System.out));
    private static StdConsole instance;

    public StdConsole() {
        this.instance = this;
    }

    public static String read() {
        try {
            return consoleReader.readLine();
        } catch (IOException e) {
            return null;
        }
    }

    public static String read(String prompt) {
        writeln(prompt);
        return read();
    }

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

    public void close() throws Exception {
        consoleReader.close();
        consoleWriter.close();
    }

    public static StdConsole getInstance() {
        return instance!=null? instance :new StdConsole();
    }
}
