package lab5;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.Stack;

import org.junit.jupiter.api.Test;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import lab5.spacemarines.Chapter;
import lab5.spacemarines.Coordinates;
import lab5.spacemarines.MeleeWeapon;
import lab5.spacemarines.SpaceMarine;
import lab5.utils.XMLhandler;

public class SpaceMarineTest {

    @Test
    public void testMarshaling() throws JAXBException {
        SpaceMarine marine = new SpaceMarine("Test Marine", new Coordinates(1.0, 2.0f), "Brave",
                MeleeWeapon.CHAIN_SWORD, new Chapter("Test Chapter", "Test World"));

        XMLhandler xmlHandler = new XMLhandler();
        xmlHandler.writeStackToXML(marine);

        JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
        Marshaller marshaller = context.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

        StringWriter writer = new StringWriter();
        marshaller.marshal(marine, writer);

        String xmlOutput = writer.toString();
        System.out.println(xmlOutput); // For debugging purposes
    }

    @Test
    public void testUnmarshaling() throws JAXBException {
        String xmlInput = "<spaceMarine><id>1</id><name>Test Marine</name><coordinates><x>1.0</x><y>2.0</y></coordinates><creationDate>2023-10-01T12:00:00</creationDate><health>100.0</health><loyal>true</loyal><achievements>Brave</achievements><meleeWeapon>CHAIN_SWORD</meleeWeapon><chapter><name>Test Chapter</name><world>Test World</world></chapter></spaceMarine>";

        XMLhandler xmlHandler = new XMLhandler();
        Stack<SpaceMarine> marineStack = xmlHandler.readStackFromXML();
        SpaceMarine unmarshalledMarine = marineStack.pop();

        JAXBContext context = JAXBContext.newInstance(SpaceMarine.class, Coordinates.class, Chapter.class);
        Unmarshaller unmarshaller = context.createUnmarshaller();

        SpaceMarine testMarine = (SpaceMarine) unmarshaller.unmarshal(new StringReader(xmlInput));

        assertEquals("Test Marine", unmarshalledMarine.getName());
        assertEquals(MeleeWeapon.CHAIN_SWORD, unmarshalledMarine.getMeleeWeapon());
    }
}