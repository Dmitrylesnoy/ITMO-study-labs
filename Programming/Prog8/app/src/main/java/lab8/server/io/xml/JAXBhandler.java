package lab8.server.io.xml;

import java.io.File;
import java.util.Collection;
import java.util.Stack;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab8.shared.io.console.StdConsole;
import lab8.shared.model.Chapter;
import lab8.shared.model.Coordinates;
import lab8.shared.model.MeleeWeapon;
import lab8.shared.model.SpaceMarine;

/**
 * JAXBhandler is responsible for handling the serialization and deserialization
 * of SpaceMarine objects and related classes to and from XML files using JAXB.
 * This class provides methods for writing a collection to an XML file and reading
 * a collection from an XML file.
 */
public class JAXBhandler {

    private String filename = "data.xml"; // Set to a hardcoded value for testing

    /**
     * Default constructor for the JAXBhandler class, initializing with the default filename.
     */
    public JAXBhandler() {
    }

    /**
     * Constructor that initializes the JAXBhandler with a specific filename.
     *
     * @param filename the name of the XML file to read from or write to
     */
    public JAXBhandler(String filename) {
        this.filename = filename;
    }

    /**
     * Writes a collection of SpaceMarine objects to an XML file.
     *
     * @param stack the collection of SpaceMarine objects to write
     */
    public void writeCollection(Collection stack) {
        try {
            JAXBContext context = JAXBContext.newInstance(Stack.class, SpaceMarine.class, Coordinates.class,
                    Chapter.class, MeleeWeapon.class);

            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

            File file = new File("data.xml");
            marshaller.marshal(stack, file);
        } catch (Exception e) {
            new StdConsole().writeln("Error while marshalling: " + e.getMessage() + " type: " + e.getClass()
                    + " - Check if the objects are properly annotated.");
        } finally {
            // Handle any cleanup if necessary
        }
    }

    /**
     * Reads a collection of SpaceMarine objects from an XML file.
     *
     * @return a stack of SpaceMarine objects read from the XML file
     */
    public Stack<SpaceMarine> readCollection() {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();

            File file = new File(filename);
            Stack<SpaceMarine> result = (Stack<SpaceMarine>) unmarshaller.unmarshal(file);
            return result;
        } catch (JAXBException e) {
            new StdConsole().writeln("Error while unmarshalling: " + e.getMessage());
            return new Stack<>();
        }
    }
}
