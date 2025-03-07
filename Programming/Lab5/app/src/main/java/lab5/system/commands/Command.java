package lab5.system.commands;

import java.io.IOException;

public interface Command {
    public void execute() throws IOException;

    public String describe();

    public default String getOutput() {
        return "";
    }
}
