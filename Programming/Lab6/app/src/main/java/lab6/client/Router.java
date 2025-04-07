package lab6.client;

import java.util.HashMap;
import java.util.Map;

import lab6.server.Worker;
import lab6.server.Collection.CollectionManager;
import lab6.server.utils.ScriptController;
import lab6.system.commands.Add;
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
import lab6.system.commands.RandomAdd;
import lab6.system.commands.RemoveByID;
import lab6.system.commands.RemoveGreater;
import lab6.system.commands.RemoveLower;
import lab6.system.commands.Show;
import lab6.system.commands.Sort;
import lab6.system.commands.UpdateId;
import lab6.system.messages.Response;
import lab6.system.messages.Status;

/**
 * The Router class is responsible for routing commands to their corresponding
 * command handlers.
 * It maintains a collection of commands and executes them based on user
 * requests.
 * This class implements the Singleton pattern to ensure that only one instance
 * of Router exists.
 */
public class Router {
    private CollectionManager cm = new CollectionManager();
    private ScriptController scriptController;
    private Worker server = new Worker();

    /**
     * Default constructor for the Router class.
     * Initializes the Router instance and loads the collection manager.
     */
    public Router() {
        cm.load();
        scriptController = new ScriptController();
    }

    /**
     * Executes the command based on the provided request.
     *
     * @param request the request containing the command and its arguments
     * @return the response after executing the command
     */
    public Response runCommand(String name, String[] args) {
        Response response;
        String output = "";
        Command cmd;
        Map<String, Command> cmds = new HashMap<>();
        cmds.put("add", new Add());
        cmds.put("add_random", new RandomAdd());
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

        name = name.toLowerCase();
        if (cmds.containsKey(name)) {
            cmd = cmds.get(name.toLowerCase());
            try {
                if (cmd.getClass() == FilterStartsWithAchievements.class)
                    ((FilterStartsWithAchievements) cmd)
                            .setArgs(args != null ? args[0] : null);
                if (cmd.getClass() == RemoveByID.class)
                    ((RemoveByID) cmd).setArgs(args != null ? Long.parseLong(args[0]) : null);
                if (cmd.getClass() == ExecuteScript.class) {
                    ((ExecuteScript) cmd).setArgs(args[0]);
                    scriptController.checkExecuting(args[0]);
                    scriptController.addScript(args[0]);
                }
                if (cmd.getClass() == RandomAdd.class)
                    ((RandomAdd) cmd).setArgs(args != null ? Integer.parseInt(args[0]) : 1);
                // StdConsole.writeln(request.getArgs()[0]);
                cmd.execute();

                if (cmd.getClass() == ExecuteScript.class)
                    scriptController.endScript();

                output = cmd.getOutput();
                cm.save();
                response = new Response(name, Status.COMPLETE, output, null);
            } catch (ArrayIndexOutOfBoundsException e) {
                response = new Response(name, Status.FAILED, null,
                        new ArrayIndexOutOfBoundsException("Missing giving arguments for command"));
            } catch (IllegalArgumentException e) {
                response = new Response(name, Status.FAILED, null,
                        new IllegalArgumentException("Wrong types for giving arguments"));
            } catch (NullPointerException e) {
                response = new Response(name, Status.FAILED, null,
                        new NullPointerException("Empty arguments for command"));
            } catch (RuntimeException e) {
                response = new Response(name, Status.FAILED, null, e);
            } catch (Exception e) {
                response = new Response(name, Status.FAILED, null, e);
            }
        } else {
            response = new Response(name, Status.FAILED, null,
                    new UnsupportedOperationException("Unknown command"));
        }

        return response;
    }
}
