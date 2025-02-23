package lab5.utils;

import java.io.File;
import java.util.Stack;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab5.spacemarines.*;

public class XMLhandler {

    // private final String filename = "data.xml";
    private final String filename = System.getenv("XMLFILENAME");

    public void writeStackToXML(Object objects) {
        try {
            // JAXBContext context = JAXBContext.newInstance(Stack.class,SpaceMarine.class, Coordinates.class, Chapter.class, MeleeWeapon.class);
            JAXBContext context = JAXBContext.newInstance(objects.getClass());

            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

            File file = new File("data.xml");
            marshaller.marshal(objects, file);
        } catch (JAXBException e) {
            System.err.println("Error while marshalling: " + e.getMessage());
        }
    }

    public Stack<SpaceMarine> readStackFromXML() {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();

            File file = new File(filename);
            @SuppressWarnings("unchecked")
            Stack<SpaceMarine> result = (Stack<SpaceMarine>) unmarshaller.unmarshal(file);
            return result;
        } catch (JAXBException e) {
            System.err.println("Error while unmarshalling: " + e.getMessage());
            return new Stack<>();
        }
    }
    public void writeStackToXML(Stack<?> objects) {

    }

    // public Stack<T> readStackFromXML() {
    // return new Stack<T>;
    // }

}
