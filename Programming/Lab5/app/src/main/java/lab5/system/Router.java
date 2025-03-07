package lab5.system;

import java.util.HashMap;
import java.util.Map;

import lab5.system.commands.*;
import lab5.system.messages.*;
import lab5.system.utils.CollectionManager;

public class Router {
    private Response response;
    private static Router instance;
    private CollectionManager cm;

    public Router() {
        Router.instance = this;
        cm = CollectionManager.getInstance();
        cm.load();
    }

    public static Router getInstance() {
        return instance == null ? new Router() : instance;
    }

    public Response runCommand(Request request) {
        String output = "";
        Command cmd;
        Map<String, Command> cmds = new HashMap<>();
        cmds.put("add", new Add());
        cmds.put("clear", new Clear());
        cmds.put("exit", new Exit());
        cmds.put("load", new Load());
        cmds.put("save", new Save());
        cmds.put("show", new Show());
        cmds.put("sort", new Sort());
        cmds.put("filterftartswithachievements", new FilterStartsWithAchievements());
        cmds.put("minbymeleeweapon", new MinByMeleeWeapon());
        cmds.put("removebyid", new RemoveByID());
        cmds.put("removegreater", new RemoveGreater());
        cmds.put("removelower", new RemoveLower());
        cmds.put("updateid", new UpdateId());
        cmds.put("prinuniqueloyal", new PrintUniqueLoyal());
        cmds.put("executescript", new ExecuteScript());
        cmds.put("help", new Help(cmds));

        String name = request.getCommand().toLowerCase();
        if (cmds.containsKey(name)) {
            cmd = cmds.get(name.toLowerCase());
            try {
                switch (name) {
                    case "filterftartswithachievements":
                        cmd = new FilterStartsWithAchievements(request.getArgs() != null ? request.getArgs()[0] : null);
                        break;
                    case "removebyid":
                        cmd = new RemoveByID(request.getArgs() != null ? Long.parseLong(request.getArgs()[0]) : null);
                        break;
                    case "executescript":
                        cmd = new ExecuteScript(request.getArgs()[0]);
                        break;
                    default:
                        break;
                }
                cmd.execute();
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
            } catch (Exception e) {
                response = new Response(name, Status.FAILED, e);
            }
        } else {
            response = new Response(name, Status.FAILED,
                    new UnsupportedOperationException("Unknown command"));
        }

        return response;
    }
}
