package lab6.system;

import java.util.Arrays;

import lab6.system.io.console.StdConsole;
import lab6.system.messages.Request;
import lab6.system.messages.Response;

/**
 * The Handler class is responsible for processing user input commands.
 * It utilizes a Router to execute commands based on requests created from user
 * input.
 * This class provides methods for running commands, making requests, and
 * managing the console interface.
 */
public class Handler {
    private Router router = new Router();
    private static Handler instance;
    private StdConsole console = new StdConsole();

    /**
     * Default constructor for the Handler class.
     */
    public Handler() {
        instance = this;
    }

    /**
     * Runs the command based on user input read from the console.
     */
    public void Run() {
        try {
            String input;

            input = console.read();

            Request request = makeRequest(input);
            Response response = router.runCommand(request);
            console.write(response.toString());
        } catch (NullPointerException e) {
            console.write("");
        } catch (Exception e) {
            console.write(e.toString());
        }
    }

    /**
     * Runs the command based on the provided request.
     *
     * @param request the request to be processed
     */
    public void Run(Request request) {
        try {
            Response response = router.runCommand(request);
            StdConsole.write(response.toString());
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
        }

    }

    /**
     * Creates a Request object from the user input string.
     *
     * @param input the user input string
     * @return the created Request object
     */
    public Request makeRequest(String input) {
        String[] inp_split;
        try {
            inp_split = input.strip().split("\\s+");
        } catch (Exception e) {
            inp_split = new String[] {};
        }
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

    /**
     * Returns the router associated with this Handler.
     *
     * @return the Router instance
     */
    public Router getRouter() {
        return this.router;
    }

    /**
     * Returns the singleton instance of the Handler.
     *
     * @return the instance of Handler
     */
    public static Handler getInstance() {
        return instance == null ? new Handler() : instance;
    }

    public StdConsole getConsole() {
        return console;
    }

    public void setConsole(StdConsole console) {
        this.console = console;
    }
}
