package lab6.system.messages;

import lab6.system.commands.Command;
import lombok.Getter;
import lombok.Setter;

/**
 * Represents a command request containing a command name and its associated arguments.
 * This class provides constructors for initializing the command and its arguments,
 * and uses Lombok annotations to generate getter and setter methods.
 */
// @Getter
// @Setter
public record Request(Command command, String[] args) {
// public class Request {
//     private String command;
//     private String[] args;

    /**
     * Constructs a Request with the specified command name.
     *
     * @param name the name of the command
     */
    // public Request(String name) {
    //     this.command = name.toLowerCase();
    // }

    /**
     * Constructs a Request with the specified command name and arguments.
     *
     * @param command the name of the command
     * @param args the arguments associated with the command
     */
    // public Request(String command, String[] args) {
    //     this.command = command.toLowerCase();
    //     this.args = args;
    // }
}
