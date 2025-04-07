package lab6.client;

public class ClientMain {
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
