package lab5.utils;

import java.io.File;
import java.util.Stack;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab5.spacemarines.SpaceMarine;

public class XMLhandler {
    private final String filename = System.getenv("XMLFILENAME");

    public void writeStackToXML(Stack<SpaceMarine> objects) {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class);
            Marshaller marshaller = context.createMarshaller();
            // marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            
            File file = new File(filename);
            marshaller.marshal(objects, file);
        } catch (JAXBException e) {
            System.err.println("Error while marshalling: " + e.getMessage());
        }
    }

    public Stack<SpaceMarine> readStackFromXML() {
        try {
            JAXBContext context = JAXBContext.newInstance(SpaceMarine.class);
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
}
