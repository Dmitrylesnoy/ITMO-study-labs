package lab8.shared.io.console;

import java.util.Deque;
import java.util.LinkedList;

import lab8.client.controllers.TerminalController;

public class ClientConsole {
    private Deque<String> inputDeque = new LinkedList<String>();
    private TerminalController terminal;
    private static ClientConsole instance;

    public ClientConsole() {
        instance = this;
        // terminal = new TerminalController();
    }

    public static ClientConsole getInstance() {
        return instance==null? instance=new ClientConsole() : instance;
    }

    public void write(String text) {
        TerminalController.write(text);
        StdConsole.write(text);
    }

    public void writeln(String text) {
        TerminalController.writeln(text);
        StdConsole.writeln(text);
    }

    public String read() {
        if (!inputDeque.isEmpty())
            return inputDeque.poll();
        return "";
    }

    public void add(String line) {
        inputDeque.push(line);
    }
}
