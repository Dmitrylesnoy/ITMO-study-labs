package lab5.system.io.file;

import java.util.Stack;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlCData;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import lab5.system.model.SpaceMarine;

@JacksonXmlRootElement(localName = "Stack")
public class StackMarine {
    @JacksonXmlProperty(localName = "SpaceMarine")
    // @JacksonXmlCData
    @JacksonXmlElementWrapper(useWrapping = false)
    private Stack<SpaceMarine> stack;

    public StackMarine(){}

    public StackMarine(Stack<SpaceMarine> stack) {
        this.stack = stack;
    }

    public Stack getStack(){
        return stack;
    }
}
