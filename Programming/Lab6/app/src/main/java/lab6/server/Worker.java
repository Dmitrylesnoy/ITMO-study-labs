package lab6.server;

import java.util.Collection;

import lab6.server.utils.ScriptController;
import lab6.system.collection.CollectionManager;
import lab6.system.commands.Command;
import lab6.system.commands.ExecuteScript;
import lab6.system.commands.Exit;
import lab6.system.commands.Save;
import lab6.system.messages.Request;
import lab6.system.messages.Response;
import lab6.system.messages.Status;

public class Worker {
    private ScriptController scriptCtrl;
    
    public Worker() {
        scriptCtrl = new ScriptController();
    }

    public Response processCommand(Request request) {
        Command cmd = request.command();
        if (request.args() != null)
            cmd.setArgs(request.args());
        Response response;
        try {
            cmd.execute();
            Save save = new Save();save.execute();
            if (cmd.getClass() == ExecuteScript.class)
                scriptCtrl.endScript();
            if (cmd.getClass() == Exit.class){
                response = new Response(cmd.getName(), Status.CLOSE, "", null);
                CollectionManager.getInstance().save();}
            else
                response = new Response(cmd.getName(), Status.COMPLETE, cmd.getOutput(), null);
        } catch (ArrayIndexOutOfBoundsException e) {
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new ArrayIndexOutOfBoundsException("Missing giving arguments for command"));
        } catch (IllegalArgumentException e) {
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new IllegalArgumentException("Wrong types for giving arguments"));
        } catch (NullPointerException e) {
            response = new Response(cmd.getName(), Status.FAILED, null,
                    new NullPointerException("Empty arguments for command"));
        } catch (RuntimeException e) {
                response = new Response(cmd.getName(), Status.FAILED, null, e);
        } catch (Exception e) {
            response = new Response(cmd.getName(), Status.FAILED, null, e);
        }
        return response;
        
    }
}
