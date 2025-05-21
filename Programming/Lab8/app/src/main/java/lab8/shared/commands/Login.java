package lab8.shared.commands;

public class Login extends Command {
    private StringBuilder output = new StringBuilder();

    public Login() {
    }

    public void execute() {
        
    }

    public String getOutput() {
        return output.toString();
    }

    public String describe() {
        return "Return date of creating collection and it's lenght";
    }

    public String getName() {
        return "Info";
    }
}
