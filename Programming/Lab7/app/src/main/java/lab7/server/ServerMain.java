package lab7.server;

import java.io.IOException;

public class ServerMain {
    public static void main(String[] args) {
        try {
            Router router = new Router();
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                router.shutdown();
                System.out.println("Server shut down gracefully");
            }));
            router.run();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}