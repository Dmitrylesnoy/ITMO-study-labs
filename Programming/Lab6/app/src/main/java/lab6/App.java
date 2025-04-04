package lab6;

import lab6.system.Handler;
import lab6.system.io.console.StdConsole;

public class App {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        
        Handler handler = new Handler();
        StdConsole.write("=>");
        handler.getConsole().add("help");
        while (true) {
            handler.Run();
        }
    }
}
