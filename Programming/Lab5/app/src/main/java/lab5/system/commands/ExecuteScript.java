package lab5.system.commands;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import lab5.system.Handler;

public class ExecuteScript implements Command {
    private String scriptFilePath;

    public ExecuteScript() {
    }

    public ExecuteScript(String scriptFilePath) {
        this.scriptFilePath = scriptFilePath;
    }

    @Override
    public void execute() throws IOException {
        try {
            File scriptFile = new File(scriptFilePath);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(scriptFile));
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                Handler.getInstance().Run(Handler.getInstance().makeRequest(line));
            }
            bufferedReader.close();
        } catch (FileNotFoundException e) {
            throw new FileNotFoundException("Script file not found");
        }
    }

    public String describe() {
        return "Execute script with programm commands";
    }
}
