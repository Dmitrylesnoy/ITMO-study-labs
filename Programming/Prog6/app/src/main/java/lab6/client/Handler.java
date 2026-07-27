package lab6.client;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import lab6.client.script.ExecuteScript;
import lab6.client.script.ScriptController;
import lab6.shared.exceptions.ScriptRecursionException;
import lab6.shared.io.console.StdConsole;
import lab6.shared.messages.Request;
import lab6.shared.messages.Response;
import lab6.shared.messages.Status;
import lab6.shared.model.builders.SpaceMarineBuilder;

/**
 * The Handler class is responsible for processing user input commands.
 * It utilizes a Router to execute commands based on requests created from user
 * input.
 * This class provides methods for running commands, making requests, and
 * managing the console interface.
 */
public class Handler {
    List<String> cmds = new ArrayList<String>();
    private NetworkClient network = new NetworkClient();

    /**
     * Default constructor for the Handler class.
     */
    public Handler() {
        // router = new Router();
        try {
            cmds.add("cmds_list");
            cmds = Arrays.asList(network.sendRequest(makeRequest("cmds_list")).output().split("\\s+"));
            StdConsole.write("=>");
        } catch (Exception e) {
            StdConsole.writeln("System error");
            StdConsole.writeln(e.toString());
            System.exit(0);
        }
    }

    /**
     * Runs the command based on user input read from the StdConsole.
     */
    public void run() {
        // Request request = makeRequest(StdConsole.read());
        try {
            Request request = makeRequest(StdConsole.read());

            if (request.command().equals("execute_script".toLowerCase())) {
                ExecuteScript execScript = new ExecuteScript((String) request.args());
                execScript.execute();
                ScriptController.getInstance().addScript((String) request.args());
                return;
            }

            Response response = network.sendRequest(request);
            StdConsole.write(response.toString());
            if (response.status() == Status.CLOSE) {
                System.exit(0);
            }
        } catch (ScriptRecursionException e) {
            StdConsole.writeln(e.toString());
            ScriptController.getInstance().clear();
            StdConsole.write("=>");
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
            StdConsole.write("=>");
        }

    }

    /**
     * Creates a Request object from the user input string.
     *
     * @param input the user input string
     * @return the created Request object
     * @throws IOException
     */
    public Request makeRequest(String input) throws IOException {
        String[] inp_split = null;
        String[] inp_args = null;
        try {
            inp_split = input.strip().split("\\s+");
        } catch (Exception e) {
            inp_split = new String[] {};
        }

        if (inp_split.length > 0 && inp_split[0].strip() != "") {
            if (inp_split.length > 1) {
                inp_args = Arrays.copyOfRange(inp_split, 1, inp_split.length);
            }
        }

        String name = inp_split[0].toLowerCase();
        String cmd;
        Object cmdArgs = null;

        if (cmds.contains(name)) {
            cmd = name;

            if (cmd.equals("add".toLowerCase()) ||
                    cmd.equals("remove_greater".toLowerCase()) ||
                    cmd.equals("remove_lower".toLowerCase()))
                cmdArgs = new SpaceMarineBuilder().build();
            if (cmd.equals("add_random".toLowerCase()))
                cmdArgs = inp_args != null ? Integer.parseInt(inp_args[0]) : 1;
            if (cmd.equals("filter_starts_with_achievements".toLowerCase()) || cmd.equals("execute_script".toLowerCase()))
                cmdArgs = inp_args != null ? inp_args[0] : null;
            if (cmd.equals("remove_by_id".toLowerCase()))
                cmdArgs = inp_args != null ? Long.parseLong(inp_args[0]) : null;
            if (cmd.equals("update_id".toLowerCase()))
                cmdArgs = new SpaceMarineBuilder().setId(Long.parseLong(inp_args[0])).build();

        } else
            throw new UnsupportedOperationException("Unknown command");
        return new Request(cmd, cmdArgs);
    }
}
