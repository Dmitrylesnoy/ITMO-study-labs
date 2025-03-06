package lab5.system.commands;

public interface Command {
    public void execute();

    public String describe();

    public default String getOutput() {
        return "";
    }
}
