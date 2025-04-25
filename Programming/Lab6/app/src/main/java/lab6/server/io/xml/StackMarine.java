package lab6.server.io.xml;

import java.util.Stack;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lab6.shared.model.SpaceMarine;

/**
 * StackMarine is a wrapper class for a stack of SpaceMarine objects.
 * This class is designed for XML serialization and deserialization using Jackson annotations.
 */
@JacksonXmlRootElement(localName = "Stack")
public class StackMarine {
    @JacksonXmlProperty(localName = "SpaceMarine")
    @JacksonXmlElementWrapper(useWrapping = false)
    private Stack<SpaceMarine> stack;

    /**
     * Default constructor for the StackMarine class, initializing an empty stack.
     */
    public StackMarine() {}

    /**
     * Constructor that initializes the StackMarine with a specific stack of SpaceMarine objects.
     *
     * @param stack the stack of SpaceMarine objects to wrap
     */
    public StackMarine(Stack<SpaceMarine> stack) {
        this.stack = stack;
    }

    /**
     * Retrieves the stack of SpaceMarine objects.
     *
     * @return the stack of SpaceMarine objects
     */
    public Stack<SpaceMarine> getStack() {
        return stack;
    }
}
