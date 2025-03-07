package lab5.system;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import lab5.system.messages.*;
import lab5.system.io.console.*;

public class Handler {
    private Router router = Router.getInstance();
    private static StdConsole console;
    private static Handler instance;

    public Handler() {
        instance = this;
    }

    public Handler(StdConsole console) {
        this.console = console;
        instance = this;
    }

    public static Handler getInstance() {
        return instance == null ? new Handler() : instance;
    }

    public void Run() {
        String input = console.read();
        Request request = makeRequest(input);
        Response response = router.runCommand(request);
        console.write(response.toString());
    }

    public void Run(Request request) {
        Response response = router.runCommand(request);
        console.write(response.toString());
    }

    public Request makeRequest(String input) {
        String[] inp_split = input.split(" ");
        Request request;
        if (inp_split.length > 0) {
            if (inp_split.length > 1) {
                request = new Request(inp_split[0], Arrays.copyOfRange(inp_split, 1, inp_split.length));
            } else {
                request = new Request(inp_split[0]);
            }
            
            return request;
        }
        return null;
    }

    public Router getRouter() {
        return this.router;
    }
}
