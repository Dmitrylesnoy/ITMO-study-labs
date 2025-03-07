package lab5.system.io.xml;

import java.io.File;
import java.util.Collection;
import java.util.Stack;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab5.system.io.console.StdConsole;
import lab5.system.model.Chapter;
import lab5.system.model.Coordinates;
import lab5.system.model.MeleeWeapon;
import lab5.system.model.SpaceMarine;

public class JAXBhandler {

    // private final String filename = "data.xml";
    private String filename = "data.xml"; // Set to a hardcoded value for testing

    public JAXBhandler() {
    }

    public JAXBhandler(String filename) {
        this.filename = filename;
    }

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
            byte a;
        }

    }

    public Stack readCollection() {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();

            File file = new File(filename);
            Stack<SpaceMarine> result = (Stack<SpaceMarine>) unmarshaller.unmarshal(file);
            return result;
        } catch (JAXBException e) {
            new StdConsole().writeln("Error while unmarshalling: " + e.getMessage());
            return new Stack();
        }
    }
}
