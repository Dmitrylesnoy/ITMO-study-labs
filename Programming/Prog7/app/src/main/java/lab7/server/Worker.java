package lab7.server;

import java.io.IOException;
import java.util.logging.Logger;

import lab7.server.io.database.DatabaseManager;
import lab7.server.utils.ScriptController;
import lab7.shared.collection.CollectionManager;
import lab7.shared.commands.Command;
import lab7.shared.commands.ExecuteScript;
import lab7.shared.commands.Exit;
import lab7.shared.commands.Load;
import lab7.shared.commands.Save;
import lab7.shared.messages.Request;
import lab7.shared.messages.Response;
import lab7.shared.messages.Status;

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
        // if (args != null) {
        cmd.setArgs(request.args());
        logger.info(String.format("[PROCESSING] Arguments set: %s, %s", request.args().toString(),
                request.args().getClass()));
        // } else
        // logger.info("[PROCESSING] Empty arguments");
        Response response = null;
        try {
            Integer user = dbManager.addUser(request.username(), request.password());
            logger.info(String.format("[PROCESSING] Setting user id " + user));
            cmd.setUser(user);

            logger.info(String.format("[PROCESSING] Start executing"));
            cmd.execute();
            logger.info(String.format("[PROCESSING] %s command executiong finished", cmd.getName()));
            Save save = new Save();
            save.execute();
            if (cmd.getClass() == ExecuteScript.class)
                scriptCtrl.endScript();
            if (cmd.getClass() == Exit.class) {
                response = new Response(cmd.getName(), Status.CLOSE, "", null);
            } else
                response = new Response(cmd.getName(), Status.COMPLETE, cmd.getOutput(), null);

        } catch (ArrayIndexOutOfBoundsException e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new ArrayIndexOutOfBoundsException("Missing giving arguments for command"));
            // } catch (IllegalArgumentException e) {
            // response = new Response(cmd.getName(), Status.FAILED, null,
            // new IllegalArgumentException("Wrong types for giving arguments"));
        } catch (RuntimeException e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        } catch (IOException e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        } catch (Exception e) {
            e.printStackTrace();
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        }

        return response;

    }
}
