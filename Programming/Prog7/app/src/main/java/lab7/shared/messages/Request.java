package lab7.shared.messages;

import java.io.Serializable;

import lab7.shared.commands.Command;

public record Request(Command command, Object args, String username, String password) implements Serializable {

    public Request(Command command, Object args, String username, String password) {
        this.command = command;
        this.args = args == null ? "" : args;
        this.username = username;
        this.password = password;
    }
}
