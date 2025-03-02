package lab5.io;

import java.io.File;
import java.util.Collection;
import java.util.Stack;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab5.spacemarines.*;

public class JAXBhandler implements Handler{

    // private final String filename = "data.xml";
    private String filename = "data.xml"; // Set to a hardcoded value for testing

    public JAXBhandler(){}
    public JAXBhandler(String filename) {
        this.filename = filename;
    }

    @Override
    public void writeCollection(Collection stack) {
        try {
            JAXBContext context = JAXBContext.newInstance(Stack.class, SpaceMarine.class, Coordinates.class,
                    Chapter.class, MeleeWeapon.class);

            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

            File file = new File("data.xml");
            marshaller.marshal(stack, file);
        } catch (Exception e) {
            System.err.println("Error while marshalling: " + e.getMessage() + " type: " + e.getClass()
                    + " - Check if the objects are properly annotated.");
        } finally {
            byte a;
        }

    }

    @Override
    public Stack readCollection() {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();

            File file = new File(filename);
            Stack<SpaceMarine> result = (Stack<SpaceMarine>) unmarshaller.unmarshal(file);
            return result;
        } catch (JAXBException e) {
            System.err.println("Error while unmarshalling: " + e.getMessage());
            return new Stack();
        }
    }
}
