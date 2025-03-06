package lab5.system.messages;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Request {
    private String command;
    private String[] args;

    public Request(String command, String[] args) {
        this.command = command;
        this.args = args;
    }
}
