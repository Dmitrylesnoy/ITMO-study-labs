package lab6.shared.exceptions;

/**
 * Exception thrown when a method receives a null argument.
 * This class extends IllegalArgumentException to indicate that
 * an invalid argument has been provided.
 */
public class NullArgumentException extends IllegalArgumentException {
    /**
     * Constructs a new NullArgumetnException with the specified detail message.
     *
     * @param message the detail message
     */
    public NullArgumentException(String message) {
        super(message);
    }

    /**
     * Returns a string representation of the exception, indicating that
     * the input must not be null.
     *
     * @return a string describing the exception
     */
    @Override
    public String toString() {
        return "Wrong entered argument. Input mustn't be Null. " + super.toString();
    }
}
