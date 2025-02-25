package lab5.commands;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

public class ExecuteSctipt implements Command {
    private String scriptFilePath;

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
                    System.out.println(line.toString());
                    line.setLength(0);
                } else {
                    line.append((char) character);
                }
            }
            if (line.length() > 0) {
                System.out.println(line.toString());
            }
            inputStreamReader.close();
        } catch (IOException e) {
            System.err.println("Error reading the script file: " + e.getMessage());
        }
    }

    @Override
    public String describe() {
        return "Execute script with programm commands";
    }
}
