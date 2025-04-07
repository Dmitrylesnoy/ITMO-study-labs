package lab6.server;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;

import lab6.server.Collection.CollectionManager;
import lab6.server.utils.ScriptController;
import lab6.system.commands.Command;
import lab6.system.commands.ExecuteScript;
import lab6.system.commands.Help;
import lab6.system.messages.Request;
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
    private CollectionManager cm;
    private static Router instance;
    private Deque<Command> cmdsQueue;

    /**
     * Default constructor for the Router class.
     * Initializes the Router instance and loads the collection manager.
     */
    public Router() {
        cm = CollectionManager.getInstance();
        cm.load();
        cmdsQueue = new ArrayDeque<Command>(1);
        cmdsQueue.add(new Help());
    }

    /**
     * Executes the command based on the provided request.
     *
     * @param request the request containing the command and its arguments
     * @return the response after executing the command
     */
    public Response runCommand(Request reaquest) {
        cmdsQueue.add(reaquest.command());
        Worker worker1 = new Worker();
        return worker1.processCommand(cmdsQueue.pop());
    }
    
    public Router getInstance(){
        return instance==null? new Router(): instance;
    }
}
