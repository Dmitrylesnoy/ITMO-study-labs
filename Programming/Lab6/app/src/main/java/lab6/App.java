package lab6;

import lab6.client.Handler;
import lab6.system.io.console.StdConsole;

public class App {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler to run a console-based program indefinitely.
     */
    public static void main(String[] args) {

        Handler handler = new Handler();
        while (true) {
            handler.run();
        }
    }
}
