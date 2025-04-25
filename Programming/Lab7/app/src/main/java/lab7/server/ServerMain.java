package lab7.server;

import java.io.IOException;

import lab7.shared.commands.AddRandom;
import lab7.shared.commands.Show;
import lab7.shared.messages.Request;

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
