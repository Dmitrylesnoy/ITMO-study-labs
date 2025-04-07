package lab6.server;

public class ServerMain {
    public static void main(String[] args) {
        Router router = new Router();
        while (true) {
            router.run();
        }
    }
}
