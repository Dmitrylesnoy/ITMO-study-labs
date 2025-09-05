package lab6.shared.messages;

import java.io.Serializable;

/**
 * Represents a response to a command, containing the command name, status,
 * output, and any exceptions that may have occurred. This class provides
 * constructors for initializing the response and formats the output based
 * on the command's status.
 */
public record Response(String name, Status status, String output, Exception e) implements Serializable{
// }class Response {
    // private String name;
    // private Status status;
    // private String output;
    // private Exception e;

    // /**
    //  * Constructs a Response with the specified command name, status, and output.
    //  *
    //  * @param name   the name of the command
    //  * @param status the status of the response
    //  * @param output the output of the command
    //  */
    // public Response(String name, Status status, String output) {
    //     this.name = name;
    //     this.output = output;
    //     this.status = status;
    // }

    // /**
    //  * Constructs a Response with the specified command name, status, and exception.
    //  *
    //  * @param name   the name of the command
    //  * @param status the status of the response
    //  * @param e      the exception that occurred
    //  */
    // public Response(String name, Status status, Exception e) {
    //     this.name = name;
    //     this.status = status;
    //     this.e = e;
    // }

    // /**
    //  * Constructs a Response with the specified status, and output for the Console IO.
    //  *
    //  * @param status the status of the response (IO)
    //  * @param output the output text
    //  */
    // public Response(Status status, String output) {
    //     this.status = status;
    //     this.output = output;
    // }

    // public Status getStatus() {
    //     return status;
    // }

    /**
     * Returns a string representation of the response, formatted based on the
     * status.
     *
     * @return a string representation of the response
     */
    @Override
    public String toString() {
        switch (status) {
            case COMPLETE:
                if (output.isEmpty()) {
                    return name + "  " + status.toString() + "\n=> ";
                }
                return output + "\n=>";
            case WARNING:
                return name + "  " + status.toString() + "\n " + output + "\n=> ";
            case FAILED:
                return name + "  " + status.toString() + "\n " + e.toString() + "\n=> ";
            case CLOSE:
                return name + "  " + status.toString() + "\n=> ";
            default:
                return status.toString() + "\n" + output + "\n=> ";
        }
    }
}
