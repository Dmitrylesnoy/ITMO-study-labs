package lab5.system.exceptions;

/**
 * Exception thrown when a method receives a null argument.
 * This class extends IllegalArgumentException to indicate that
 * an invalid argument has been provided.
 */
public class ScriptRecursionException extends RuntimeException {
    /**
     * Constructs a new ScriptRecursionException with the specified detail message.
     *
     * @param message the detail message
     */
    public ScriptRecursionException(String message) {
        super(message);
    }
}
