package lab5.system.commands;

public class Exit implements Command {

    @Override
    public void execute() {
        System.exit(0);
    }

    public String describe() {
        return "Break down the programm process without saving collection in file";
    }

}
