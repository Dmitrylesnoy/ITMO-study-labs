package lab5.io;

import java.io.File;
import java.util.Collection;
import java.util.Stack;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;

import lab5.spacemarines.SpaceMarine;

public class XMLhandler { // implements Handler {
    private String filename = "data.xml";

    public XMLhandler(){}

    public XMLhandler(String filename) {
        this.filename = filename;
    }
    

    // @Override
    public void writeCollection(Stack<SpaceMarine> stack) {
        try {
            // Stack<SpaceMarine> stackMarine = new Stack<>();
            // stackMarine.addAll(stack);
            XmlMapper xmlMapper = new XmlMapper();
            xmlMapper.writer().withDefaultPrettyPrinter().writeValue(new File(filename), stack);
            File file = new File(filename);
            // assertNotNull(file);
        } catch (Exception e) {
            System.err.println(e.toString());
        }
    }

    public Collection readCollection() {
        try {
            File file = new File(filename);
            XmlMapper xmlMapper = new XmlMapper();
            Stack<SpaceMarine> collection = xmlMapper.readValue(file, Stack.class);
            // assertTrue(value.getX() == 1 && value.getY() == 2);
            // Stack stack = new Stack<>();
            return collection;
        } catch (Exception e) {
            System.err.println(e.toString());
            return null;
        }
    }


}
