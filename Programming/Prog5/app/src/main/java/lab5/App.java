package lab5;

import lab5.system.Handler;
import lab5.system.io.console.StdConsole;

public class App {
    /**
     * The main function initializes a Handler to run a console-based program.
     */
    public static void main(String[] args) {
        
        Handler handler = new Handler();
        StdConsole.write("=>");
        StdConsole.add("help");
        while (true) {
            
            handler.Run();
        }
    }
}
