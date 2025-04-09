package lab6.server;

import java.io.IOException;
import java.util.logging.Logger;

import lab6.server.utils.ScriptController;
import lab6.system.collection.CollectionManager;
import lab6.system.commands.*;
import lab6.system.io.console.StdConsole;
import lab6.system.messages.Request;
import lab6.system.messages.Response;
import lab6.system.messages.Status;

public class Worker {
    private static final Logger logger = Logger.getLogger(Worker.class.getName());
    private ScriptController scriptCtrl;
    private CollectionManager cm = CollectionManager.getInstance();

    public Worker() {
        scriptCtrl = new ScriptController();
        logger.info("[INIT] Initialized Collection Manager");
        Load load = new Load();
        logger.info("[INIT] Initialized load command");
        load.execute();
        logger.info("[INIT] Collection loaded");
    }

    public Response processCommand(Request request) {
        Command cmd = request.command();
        // if (args != null) {
        cmd.setArgs(request.args());
        logger.info(String.format("[PROCESSING] Arguments set: %s, %s", request.args().toString(),request.args().getClass()));
        // } else
            // logger.info("[PROCESSING] Empty arguments");
        Response response = null;
        try {
            cmd.execute();
            logger.info(String.format("[PROCESSING] %s command executiong finished",cmd.getName()));
            Save save = new Save();
            save.execute();
            if (cmd.getClass() == ExecuteScript.class)
                scriptCtrl.endScript();
            if (cmd.getClass() == Exit.class) {
                response = new Response(cmd.getName(), Status.CLOSE, "", null);
            } else
                response = new Response(cmd.getName(), Status.COMPLETE, cmd.getOutput(), null);

        } catch (ArrayIndexOutOfBoundsException e) {
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new ArrayIndexOutOfBoundsException("Missing giving arguments for command"));
        } catch (IllegalArgumentException e) {
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new IllegalArgumentException("Wrong types for giving arguments"));
        } catch (RuntimeException e) {
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        } catch (IOException e) {
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        } catch (Exception e) {
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        }

        return response;

    }
}
