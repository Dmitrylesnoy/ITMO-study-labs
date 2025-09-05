package lab8.server;

import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.Logger;

import lab8.server.io.database.DatabaseManager;
import lab8.server.utils.ScriptController;
import lab8.shared.collection.CollectionManager;
import lab8.shared.commands.Command;
import lab8.shared.commands.ExecuteScript;
import lab8.shared.commands.Exit;
import lab8.shared.commands.Load;
import lab8.shared.commands.LoadPart;
import lab8.shared.commands.Login;
import lab8.shared.commands.Save;
import lab8.shared.commands.Show;
import lab8.shared.messages.Request;
import lab8.shared.messages.Response;
import lab8.shared.messages.Status;
import lab8.shared.model.SpaceMarine;

public class Worker {
    private static final Logger logger = Logger.getLogger(Worker.class.getName());
    private ScriptController scriptCtrl;
    private DatabaseManager dbManager;

    public Worker() {
        scriptCtrl = new ScriptController();
        dbManager = CollectionManager.getInstance().getDataBaseManager();
        logger.info("[INIT] Initialized Managers");
        Load load = new Load();
        logger.info("[INIT] Initialized load command");
        load.execute();
        logger.info("[INIT] Collection loaded");
    }

    public Response processCommand(Request request) {
        Command cmd = request.command();
 
        cmd.setArgs(request.args());
        logger.info(String.format("[PROCESSING] Arguments set: %s, %s", request.args().toString(),
                request.args().getClass()));

        Response response = null;
        try {
            Integer user = dbManager.addUser(request.username(), request.password());
            if (cmd.getClass().equals(Login.class)) {
                return new Response("Login", Status.COMPLETE, "login", null, null);
            }

            logger.info(String.format("[PROCESSING] Setting user id " + user));
            cmd.setUser(user);

            logger.info(String.format("[PROCESSING] Start executing"));
            cmd.execute();

            logger.info(String.format("[PROCESSING] %s command executiong finished", cmd.getName()));
            Save save = new Save();
            save.execute();

            if (cmd.getClass() == ExecuteScript.class)
                scriptCtrl.endScript();
            if (cmd.getClass() == Exit.class) 
                response = new Response(cmd.getName(), Status.CLOSE, "", null, null);
            if (cmd.getClass().equals(LoadPart.class)) {
                if (cmd.getOutput().equals("FULL"))
                    response = new Response(cmd.getName(), Status.COMPLETE, "FULL", null, ((LoadPart) cmd).getPart());
                else
                    response = new Response(cmd.getName(), Status.WAITNEXT, cmd.getOutput(), null, ((LoadPart) cmd).getPart());
            } else
                response = new Response(cmd.getName(), Status.COMPLETE, cmd.getOutput(), null, null);

        } catch (ArrayIndexOutOfBoundsException e) {
            e.printStackTrace();

            response = new Response(cmd.getName(), Status.FAILED, null,
                    new ArrayIndexOutOfBoundsException("Missing giving arguments for command"), null);
            // } catch (IllegalArgumentException e) {
            // response = new Response(cmd.getName(), Status.FAILED, null,
            // new IllegalArgumentException("Wrong types for giving arguments"));
        } catch (RuntimeException e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e, null);
        } catch (IOException e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e, null);
        } catch (Exception e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e, null);
        }

        return response;

    }
}
