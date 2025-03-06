package lab5.system.io.file;

import java.io.File;
import java.util.Stack;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;

import lab5.system.io.Console.StdConsole;
import lab5.system.model.SpaceMarine;

public class XMLhandler {
    private static String filename;
    static {
        filename = System.getenv("FILENAME");
    }

    public XMLhandler() {

    }

    public XMLhandler(String filename) {
        this.filename = filename;
    }

    public static void writeCollection(Stack<SpaceMarine> stack) {
        try {
            StackMarine stMarine = new StackMarine(stack);
            XmlMapper xmlMapper = new XmlMapper();
            xmlMapper.writer().withDefaultPrettyPrinter().writeValue(new File(filename), stMarine);
            File file = new File(filename);
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
        }
    }

    public static Stack<SpaceMarine> readCollection() {
        try {
            File file = new File(filename);
            XmlMapper xmlMapper = new XmlMapper();
            xmlMapper.setSerializationInclusion(Include.NON_NULL);
            StackMarine container = xmlMapper.readValue(file, StackMarine.class);
            Stack<SpaceMarine> stack = container.getStack();
            // assertTrue(value.getX() == 1 && value.getY() == 2);
            // Stack stack = new Stack<>();
            return stack;
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
            return null;
        }
    }

}
