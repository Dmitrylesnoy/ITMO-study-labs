package lab6.client;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import lab6.system.commands.Add;
import lab6.system.commands.AddRandom;
import lab6.system.commands.Clear;
import lab6.system.commands.Command;
import lab6.system.commands.ExecuteScript;
import lab6.system.commands.Exit;
import lab6.system.commands.FilterStartsWithAchievements;
import lab6.system.commands.Help;
import lab6.system.commands.Info;
import lab6.system.commands.Load;
import lab6.system.commands.MinByMeleeWeapon;
import lab6.system.commands.PrintUniqueLoyal;
import lab6.system.commands.RemoveByID;
import lab6.system.commands.RemoveGreater;
import lab6.system.commands.RemoveLower;
import lab6.system.commands.Show;
import lab6.system.commands.Sort;
import lab6.system.commands.UpdateId;
import lab6.system.io.console.StdConsole;
import lab6.system.messages.Request;
import lab6.system.messages.Response;
import lab6.system.messages.Status;
import lab6.system.model.builders.SpaceMarineBuilder;

/**
 * The Handler class is responsible for processing user input commands.
 * It utilizes a Router to execute commands based on requests created from user
 * input.
 * This class provides methods for running commands, making requests, and
 * managing the console interface.
 */
public class Handler {
    private StdConsole console;
    Map<String, Command> cmds = new HashMap<>();
    private NetworkClient network = new NetworkClient();

    /**
     * Default constructor for the Handler class.
     */
    public Handler() {
        // router = new Router();
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
  
            Response response = network.sendRequest(request);
            console.write(response.toString());
            if (response.status() == Status.CLOSE) {
                request.command().execute();
                System.exit(0);
            }
        } catch (NullPointerException e) {
            console.write("=>");
        } catch (UnsupportedOperationException e) {
            console.writeln("Recursion detected, unsopported opetarion. Execution canceled");
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
        Command cmd;
        Object cmdArgs = null;

        if (cmds.containsKey(name)) {
            cmd = cmds.get(name);

            if (cmd.getClass().equals(Add.class) ||
                    cmd.getClass().equals(RemoveGreater.class) ||
                    cmd.getClass().equals(RemoveLower.class))
                cmdArgs = new SpaceMarineBuilder().build();
            if (cmd.getClass().equals(AddRandom.class))
                cmdArgs = inp_args != null ? Integer.parseInt(inp_args[0]) : 1;
            if (cmd.getClass().equals(FilterStartsWithAchievements.class))
                cmdArgs = inp_args != null ? inp_args[0] : null;
            if (cmd.getClass().equals(RemoveByID.class))
                cmdArgs = inp_args != null ? Long.parseLong(inp_args[0]) : null;
            if (cmd.getClass().equals(UpdateId.class))
                cmdArgs = new SpaceMarineBuilder().setID(Long.parseLong(inp_args[0])).build();
            if (cmd.getClass().equals(ExecuteScript.class)) {
                cmdArgs = inp_args != null ? inp_args[0] : null;
                cmd.setArgs(cmdArgs).execute();
            }
        } else
            throw new UnsupportedOperationException("Unknown command");

        return new Request(cmd, cmdArgs);
    }
}
