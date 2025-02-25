package lab5.commands;

public class Exit implements Command{

    @Override
    public void execute() {
        System.exit(0);
    }

    @Override
    public String describe() {
        return "Break down the programm process without saving collection in file";
    }

}
