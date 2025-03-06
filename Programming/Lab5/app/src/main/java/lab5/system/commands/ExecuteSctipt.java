package lab5.system.commands;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import lab5.system.io.Console.StdConsole;

public class ExecuteSctipt implements Command {
    private String scriptFilePath;
    private ArrayList input = new ArrayList<String>();

    public ExecuteSctipt(String scriptFilePath) {
        this.scriptFilePath = scriptFilePath;
    }

    @Override
    public void execute() {
        try {
            File scriptFile = new File(scriptFilePath);
            InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(scriptFile));
            StringBuilder line = new StringBuilder();
            int character;
            while ((character = inputStreamReader.read()) != -1) {
                if (character == '\n') {
                    input.add(line.toString());
                    line.setLength(0);
                } else {
                    line.append((char) character);
                }
            }
            if (line.length() > 0) {
                input.add(line.toString());
            }
            inputStreamReader.close();
        } catch (IOException e) {
            StdConsole.writeln("Error reading the script file: " + e.getMessage());
        }
    }

    @Override
    public String describe() {
        return "Execute script with programm commands";
    }
}
