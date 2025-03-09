package lab5.system.io.xml;

import java.io.File;
import java.util.Stack;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;

import lab5.system.io.console.StdConsole;
import lab5.system.model.SpaceMarine;

/**
 * XMLhandler is responsible for handling the serialization and deserialization
 * of SpaceMarine objects to and from XML files using Jackson's XmlMapper.
 * This class provides methods for writing a collection to an XML file and reading
 * a collection from an XML file.
 */
public class XMLhandler {
    private static String filename;

    static {
        filename = System.getenv("FILENAME");
    }

    /**
     * Default constructor for the XMLhandler class, initializing with the default filename.
     */
    public XMLhandler() {
    }

    /**
     * Constructor that initializes the XMLhandler with a specific filename.
     *
     * @param filename the name of the XML file to read from or write to
     */
    public XMLhandler(String filename) {
        this.filename = filename;
    }

    /**
     * Writes a collection of SpaceMarine objects to an XML file.
     *
     * @param stack the collection of SpaceMarine objects to write
     */
    public static void writeCollection(Stack<SpaceMarine> stack) {
        try {
            StackMarine stMarine = new StackMarine(stack);
            XmlMapper xmlMapper = new XmlMapper();
            xmlMapper.writer().withDefaultPrettyPrinter().writeValue(new File(filename), stMarine);
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
        }
    }

    /**
     * Reads a collection of SpaceMarine objects from an XML file.
     *
     * @return a stack of SpaceMarine objects read from the XML file, or null if an error occurs
     */
    public static Stack<SpaceMarine> readCollection() {
        try {
            File file = new File(filename);
            XmlMapper xmlMapper = new XmlMapper();
            xmlMapper.setSerializationInclusion(Include.NON_NULL);
            StackMarine container = xmlMapper.readValue(file, StackMarine.class);
            return container.getStack();
        } catch (Exception e) {
            StdConsole.writeln(e.toString());
            return null;
        }
    }
}
