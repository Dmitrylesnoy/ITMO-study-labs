package lab5.system;

import java.util.HashMap;
import java.util.Map;

import lab5.system.commands.*;
import lab5.system.io.console.StdConsole;
import lab5.system.messages.*;
import lab5.system.utils.CollectionManager;
import lab5.system.utils.ScriptController;

/**
 * The Router class is responsible for routing commands to their corresponding command handlers.
 * It maintains a collection of commands and executes them based on user requests.
 * This class implements the Singleton pattern to ensure that only one instance of Router exists.
 */
public class Router {
    private static long indexer=0;
    private Response response;
    private CollectionManager cm;
    private ScriptController scriptController;

    /**
     * Default constructor for the Router class.
     * Initializes the Router instance and loads the collection manager.
     */
    public Router() {
        cm = CollectionManager.getInstance();
        cm.load();
        scriptController = new ScriptController();
    }

    /**
     * Executes the command based on the provided request.
     *
     * @param request the request containing the command and its arguments
     * @return the response after executing the command
     */
    public Response runCommand(Request request) {
        String output = "";
        Command cmd;
        Map<String, Command> cmds = new HashMap<>();
        cmds.put("add", new Add());
        cmds.put("clear", new Clear());
        cmds.put("exit", new Exit());
        cmds.put("load", new Load());
        cmds.put("save", new Save());
        cmds.put("info", new Info());
        cmds.put("show", new Show());
        cmds.put("sort", new Sort());
        cmds.put("filter_starts_with_achievements", new FilterStartsWithAchievements());
        cmds.put("min_by_meleeweapon", new MinByMeleeWeapon());
        cmds.put("removeby_id", new RemoveByID());
        cmds.put("remove_greater", new RemoveGreater());
        cmds.put("remove_lower", new RemoveLower());
        cmds.put("update_id", new UpdateId());
        cmds.put("print_unique_loyal", new PrintUniqueLoyal());
        cmds.put("execute_script", new ExecuteScript());
        cmds.put("help", new Help(cmds));

        String name = request.getCommand().toLowerCase();
        if (cmds.containsKey(name)) {
            cmd = cmds.get(name.toLowerCase());
            try {
                if (cmd.getClass() == FilterStartsWithAchievements.class)
                    ((FilterStartsWithAchievements) cmd)
                            .setArgs(request.getArgs() != null ? request.getArgs()[0] : null);
                if (cmd.getClass() == RemoveByID.class)
                    ((RemoveByID) cmd).setArgs(request.getArgs() != null ? Long.parseLong(request.getArgs()[0]) : null);
                if (cmd.getClass() == ExecuteScript.class){
                    ((ExecuteScript) cmd).setArgs(request.getArgs()[0]);
                    scriptController.addScript(request.getArgs()[0]);
                    scriptController.checkExecuting(request.getArgs()[0]);

                }
                // StdConsole.writeln(request.getArgs()[0]);
                cmd.execute();

                if (cmd.getClass() == ExecuteScript.class)
                    scriptController.endScript();
                
                output = cmd.getOutput();
                response = new Response(name, Status.COMPLETE, output);
            } catch (ArrayIndexOutOfBoundsException e) {
                response = new Response(name, Status.FAILED,
                        new ArrayIndexOutOfBoundsException("Missing giving arguments for command"));
            } catch (IllegalArgumentException e) {
                response = new Response(name, Status.FAILED,
                        new IllegalArgumentException("Wrong types for giving arguments"));
            } catch (NullPointerException e) {
                response = new Response(name, Status.FAILED,
                        new NullPointerException("Empty arguments for command"));
            } catch (RuntimeException e) {
                response = new Response(name, Status.FAILED,
                        new RuntimeException("Script already exevcuting"));
            } catch (Exception e) {
                response = new Response(name, Status.FAILED, e);
            }
        } else {
            response = new Response(name, Status.FAILED,
                    new UnsupportedOperationException("Unknown command"));
        }

        return response;
    }
    
    public static long getNextId(){
        indexer = indexer + 1;
        return indexer;
    }

    public static void setIndexer(long id) {
        indexer = id;
    }
}
