package lab6.server;

import java.io.IOException;

public class ServerMain {
    public static void main(String[] args) throws IOException {
        Router router = new Router();
        // router.runCommand(new Request(new Show(), null));
        // router.runCommand(new Request(new AddRandom(), 1));
        // router.runCommand(new Request(new Show(), null));
        // NullPointerException -> Show.execute
        while (true) {
            router.run();
        }
    }
}
