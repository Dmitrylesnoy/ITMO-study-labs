package lab8.shared.messages;

/**
 * Enumeration representing the possible statuses of a command response.
 * Each status indicates the result of the command execution.
 */
public enum Status {
    COMPLETE("Completed successfully"),
    WAITNEXT("Responce sendind by a part"),
    WARNING("Completed with warnings"),
    FAILED("Execution failed"),
    INPUT("Input message"),
    OUTPUT("Output message"),
    CLOSE("Clocing session");

    private Status(String string) {
        // Constructor for the enumeration
    }
}
