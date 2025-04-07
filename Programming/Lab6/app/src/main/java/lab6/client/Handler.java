package lab6.client;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import lab6.server.Router;
import lab6.system.commands.*;
import lab6.system.io.console.StdConsole;
import lab6.system.messages.*;
import lab6.system.model.builders.SpaceMarineBuilder;

/**
 * The Handler class is responsible for processing user input commands.
 * It utilizes a Router to execute commands based on requests created from user
 * input.
 * This class provides methods for running commands, making requests, and
 * managing the console interface.
 */
public class Handler {
    private Router router;
    private StdConsole console;
    Map<String, Command> cmds = new HashMap<>();

    /**
     * Default constructor for the Handler class.
     */
    public Handler() {
        router = new Router();
        console = new StdConsole();
        console.write("=>");
        console.add("help");

        cmds.put("add", new Add());
        cmds.put("add_random", new AddRandom());
        cmds.put("clear", new Clear());
        cmds.put("exit", new Exit());
        cmds.put("load", new Load());
        // cmds.put("save", new Save());
        cmds.put("info", new Info());
        cmds.put("show", new Show());
        cmds.put("sort", new Sort());
        cmds.put("filter_starts_with_achievements", new FilterStartsWithAchievements());
        cmds.put("min_by_meleeweapon", new MinByMeleeWeapon());
        cmds.put("remove_by_id", new RemoveByID());
        cmds.put("remove_greater", new RemoveGreater());
        cmds.put("remove_lower", new RemoveLower());
        cmds.put("update_id", new UpdateId());
        cmds.put("print_unique_loyal", new PrintUniqueLoyal());
        cmds.put("execute_script", new ExecuteScript());
        cmds.put("help", new Help(cmds));
    }

    /**
     * Runs the command based on user input read from the console.
     */
    public void run() {
        // Request request = makeRequest(console.read());
        try {
            Request request = makeRequest(console.read());
            Response response = router.runCommand(request);
            console.write(response.toString());
            if (response.status() == Status.CLOSE) {
                request.command().execute();
            }
        } catch (NullPointerException e) {
            console.write("=>");
        } catch (Exception e) {
            console.writeln(e.toString());
            console.write("=>");
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
        String[] inp_split;
        try {
            inp_split = input.strip().split("\\s+");
        } catch (Exception e) {
            inp_split = new String[] {};
        }

        if (inp_split.length > 0 && inp_split[0].strip() != "") {
            String name = inp_split[0];
            Command cmd;
            if (inp_split.length > 1) {
                String[] agrs = Arrays.copyOfRange(inp_split, 1, inp_split.length);
                cmd = setCommand(name, agrs);

                return new Request(cmd, agrs);
            } else {
                return new Request(setCommand(name, null), null);
            }
        } else
            return null;
    }

    public Command setCommand(String name, String[] args) {
        if (cmds.containsKey(name)) {
            Command cmd = cmds.get(name);

            if (cmd.getClass().equals(Add.class))
                ((Add) cmd).setArgs(new SpaceMarineBuilder().build());
            if (cmd.getClass().equals(AddRandom.class))
                ((AddRandom) cmd).setArgs(args != null ? Integer.parseInt(args[0]) : 1);
            if (cmd.getClass().equals(FilterStartsWithAchievements.class))
                ((FilterStartsWithAchievements) cmd).setArgs(args != null ? args[0] : null);
            if (cmd.getClass().equals(RemoveByID.class))
                ((RemoveByID) cmd).setArgs(args != null ? Long.parseLong(args[0]) : null);
            if (cmd.getClass().equals(ExecuteScript.class))
                ((ExecuteScript) cmd).setArgs(args != null ? args[0] : null);

            return cmd;
        
        } else {
            throw new UnsupportedOperationException("Unknown command");
        }
    }
}
