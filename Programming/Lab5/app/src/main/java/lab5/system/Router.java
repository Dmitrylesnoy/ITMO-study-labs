package lab5.system;

import lab5.system.commands.*;
import lab5.system.messages.Request;
import lab5.system.messages.Response;
import lab5.system.messages.Status;
import lab5.system.model.SpaceMarine;
import lab5.system.model.builders.SpaceMarineBuilder;
import lab5.system.utils.CollectionManager;

public class Router {
    private CollectionManager cm;
    private Response response;

    public Router(CollectionManager cm) {
        this.cm = cm;
    }

    public void runCommand(Request request) {
        String output = "";
        Command cmd;
        try {
            switch (request.getCommand()) {
                case "add":
                    SpaceMarine addMarine = new SpaceMarineBuilder().build();
                    new Add(cm, addMarine).execute();
                    break;
                case "clear":
                    new Clear(cm).execute();
                    break;
                case "exit":
                    new Exit().execute();
                    break;
                case "load":
                    new Load(cm).execute();
                    break;
                case "save":
                    new Save(cm).execute();
                    break;
                case "show":
                    cmd = new Show(cm);
                    cmd.execute();
                    output = cmd.getOutput();
                    break;
                case "sort":
                    new Sort(cm).execute();
                    break;
                case "filter_starts_with_achievements":
                    if (request.getArgs().length < 1) {
                        throw new IllegalArgumentException("Missing substring argument for filter.");
                    }
                    cmd = new FilterStartsWithAchievements(cm, request.getArgs()[0]);
                    cmd.execute();
                    output = cmd.getOutput();
                    break;
                case "min_by_melee_weapon":
                    cmd = new MinByMeleeWeapon(cm);
                    cmd.execute();
                    output = cmd.getOutput();
                    break;
                case "remove_by_id":
                    if (request.getArgs().length < 1) {
                        throw new IllegalArgumentException("Missing ID argument for remove.");
                    }
                    new RemoveByID(cm, Long.parseLong(request.getArgs()[0])).execute();
                    break;
                case "remove_greater":
                    SpaceMarine greaterMarine = new SpaceMarineBuilder().build();
                    new RemoveGreater(cm, greaterMarine).execute();
                    break;
                case "remove_lower":
                    SpaceMarine lowerMarine = new SpaceMarineBuilder().build();
                    new RemoveLower(cm, lowerMarine).execute();
                    break;
                case "update_id":
                    SpaceMarine updateMarine = new SpaceMarineBuilder().build();
                    new UpdateId(cm, updateMarine).execute();
                    break;
                case "print_unique_loyal":
                    cmd = new PrintUniqueLoyal(cm);
                    cmd.execute();
                    output = cmd.getOutput();
                    break;
                default:
                    throw new IllegalArgumentException(
                            "Unknown command: " + request.getCommand() + ". With Args: " + request.getArgs());
            }
            response = new Response(Status.COMPLETE, output);
        } catch (Exception e) {
            response = new Response(Status.FAILED, e);
        }
    }

    public Response getResponse() {
        return response;
    }
}
