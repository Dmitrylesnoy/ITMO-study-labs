package lab6.client.script;

import java.util.Stack;

import lab6.shared.exceptions.ScriptRecursionException;
import lab6.shared.io.console.StdConsole;


public class ScriptController {
    private Stack<String> scripts;
    private static ScriptController instance;

    public ScriptController() {
        scripts = new Stack<String>();
    }
    
    public void checkExecuting(String name) {
        if (scripts.contains(name)) {
            throw new ScriptRecursionException("Script " + name + " is already executing");
        }
    }

    public void addScript(String script) {
        checkExecuting(script);
        scripts.add(script);
    }

    public void endScript() {
        scripts.pop();
    }

    public void clear() {
        scripts.clear();
        StdConsole.clear();
    }

    public static ScriptController getInstance() {
        return instance==null ? instance  = new ScriptController() : instance;
    }
}
