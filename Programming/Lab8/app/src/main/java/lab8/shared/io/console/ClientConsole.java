package lab8.shared.io.console;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import lab8.client.controllers.TerminalController;

public class ClientConsole {
    private static BlockingQueue<String> inputDeque = new LinkedBlockingQueue<String>();
    private static ClientConsole instance;

    public ClientConsole() {
        instance = this;
        // terminal = new TerminalController();
    }

    public static ClientConsole getInstance() {
        return instance==null? instance=new ClientConsole() : instance;
    }

    public static void write(String text) {
        TerminalController.write(text);
        StdConsole.write(text);
    }

    public static void writeln(String text) {
        TerminalController.writeln(text);
        StdConsole.writeln(text);
    }

    public static String read() {
        String[] holder = new String[1];
        Thread waitingInpThread = new Thread(() -> {
            try {
                holder[0] = inputDeque.take();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });
        waitingInpThread.start();
        try {
            waitingInpThread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        String input = holder[0];
        
        // if (!inputDeque.isEmpty())
        //     return inputDeque.take();
        return input;
    }

    public static String read(String text) {
        writeln(text);
        return read();
    }

    public static void add(String line) {
        if (line!=null)
            try {
                inputDeque.put(line);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
    }
}
