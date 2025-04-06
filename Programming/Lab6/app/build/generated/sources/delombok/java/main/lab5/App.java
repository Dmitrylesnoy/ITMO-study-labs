package lab5;

import lab5.system.Handler;
import lab5.system.io.console.StdConsole;

public class App {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        
        Handler handler = new Handler();
        StdConsole.write("=>");
        while (true) {
            handler.Run();
        }
    }
}
