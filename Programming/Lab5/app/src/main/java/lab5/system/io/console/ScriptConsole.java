package lab5.system.io.console;

import java.util.LinkedList;
import java.util.Queue;

public class ScriptConsole extends StdConsole {
    private static Queue<String> data=new LinkedList<String>();
    private static ScriptConsole instance;

    public ScriptConsole() {
        this.instance = this;
    }

    public static String read() {
        return data.poll();
    }
    
    public static void write(String line) {
        data.add(line);
    }
    
    public static void writeln(String line) {
        data.add(line);
    }

    public static ScriptConsole getInstance() {
        return instance != null ? instance : new ScriptConsole();
    }
}
