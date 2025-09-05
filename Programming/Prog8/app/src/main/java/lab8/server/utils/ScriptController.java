package lab8.server.utils;

import java.util.Stack;

public class ScriptController {
    private Stack<String> scripts;

    public ScriptController() {
        scripts = new Stack<String>();
    }
    
    public void checkExecuting(String name) {
        if (scripts.contains(name)) {
            throw new RuntimeException("Script " + name + " is already executing");
        }
    }

    public void addScript(String script) {
        scripts.add(script);
    }

    public void endScript() {
        scripts.pop();
    }
}
