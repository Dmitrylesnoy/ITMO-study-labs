package lab6.shared.collection;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Stack;

import lab6.server.Router;
import lab6.server.io.xml.XMLhandler;
import lab6.shared.io.console.StdConsole;
import lab6.shared.model.SpaceMarine;

/**
 * Manages a collection of SpaceMarine objects using a stack.
 * This class provides methods for adding to the collection, saving it to an XML
 * file,
 * and loading it from an XML file. It implements the Singleton pattern to
 * ensure
 * that only one instance of CollectionManager exists.
 */
public class CollectionManager {
    private Stack<SpaceMarine> myStack = new Stack<SpaceMarine>();
    private static CollectionManager instance;
    private IDgenerator idgenerator = new IDgenerator();
    private XMLhandler xmlHandler = new XMLhandler("data.xml");

    /**
     * Default constructor for the CollectionManager class.
     * Initializes the instance of CollectionManager.
     */
    public CollectionManager() {
        CollectionManager.instance = this;
    }

    /**
     * Returns the singleton instance of CollectionManager.
     *
     * @return the instance of CollectionManager
     */
    public static CollectionManager getInstance() {
        return instance == null ? new CollectionManager() : instance;
    }

    /**
     * Returns the current collection of SpaceMarine objects.
     *
     * @return the stack of SpaceMarine objects
     */
    public Stack<SpaceMarine> getCollection() {
        return myStack;
    }

    /**
     * Sets a new collection of SpaceMarine objects.
     *
     * @param newStack the new stack of SpaceMarine objects
     */
    public void setCollection(Stack<SpaceMarine> newStack) {
        this.myStack = newStack;
        idgenerator.updateIndexer(myStack);
    }

    /**
     * Saves the current collection of SpaceMarine objects to an XML file.
     */
    public void save() {
        xmlHandler.writeCollection(this.myStack);
    }

    /**
     * Loads the collection of SpaceMarine objects from an XML file.
     */
    public void load() {

        this.myStack = (Stack<SpaceMarine>) xmlHandler.readCollection() == null ? new Stack<SpaceMarine>()
                : (Stack<SpaceMarine>) xmlHandler.readCollection();
        StdConsole.writeln("loaded stack, start indexing");
        idgenerator.updateIndexer(myStack);
    }

    /**
     * Retrieves the creation date of the XML file.
     *
     * @return the creation date as a string
     */
    public String getTime() {
        try {
            Path file = Paths.get(xmlHandler.getName());
            BasicFileAttributes attr = Files.readAttributes(file, BasicFileAttributes.class);
            return "Creation time: " + attr.creationTime();
        } catch (Exception e) {
            return "Error retrieving creation time: " + e.getMessage();
        }
    }

    /**
     * Adds a SpaceMarine object to the collection.
     *
     * @param marine the SpaceMarine object to add
     */
    public void Add(SpaceMarine marine) {
        marine.setCreationDate(new java.util.Date());
        marine.setId(idgenerator.getNextId());
        this.myStack.add(marine);
    }

    public Long nextId() {
        return idgenerator.getNextId();
    }
}
