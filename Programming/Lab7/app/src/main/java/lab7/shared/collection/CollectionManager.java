package lab7.shared.collection;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Stack;
import java.util.concurrent.locks.ReentrantLock;

import lab7.server.io.database.DatabaseManager;
import lab7.server.io.xml.XMLhandler;
import lab7.shared.model.SpaceMarine;

/**
 * Manages a collection of SpaceMarine objects using a stack.
 * This class provides methods for adding to the collection, saving it to an XML
 * file, and loading it from an XML file. It extends the Singleton pattern to
 * ensure that only one instance of CollectionManager exists.
 */
public class CollectionManager {
    private Stack<SpaceMarine> myStack = new Stack<SpaceMarine>();
    private static CollectionManager instance;
    private IDgenerator idgenerator = new IDgenerator();
    private XMLhandler xmlHandler = new XMLhandler("data.xml");
    private DatabaseManager dbManager = new DatabaseManager(
                        "jdbc:postgresql://pg/studs", "s466513",
                        System.getProperty("PGPASS"));
    private final ReentrantLock lock = new ReentrantLock();

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
        lock.lock();
        try {
            return this.myStack;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Sets a new collection of SpaceMarine objects.
     *
     * @param newStack the new stack of SpaceMarine objects
     */
    public void setCollection(Stack<SpaceMarine> newStack) {
        lock.lock();
        try {
            this.myStack = newStack;
            idgenerator.updateIndexer(myStack);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Saves the current collection of SpaceMarine objects to an XML file and
     * database.
     */
    public void save() {
        lock.lock();
        try {
            XMLhandler.writeCollection(this.myStack);
            try {
                dbManager.write(new ArrayList<>(this.myStack));
            } catch (Exception e) {
                e.printStackTrace();
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Loads the collection of SpaceMarine objects from the database.
     */
    public void load() {
        lock.lock();
        this.myStack = new Stack<SpaceMarine>();
        try {
            myStack.addAll(dbManager.read());
            idgenerator.updateIndexer(myStack);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Retrieves the creation date of the XML file.
     *
     * @return the creation date as a string
     */
    public String getTime() {
        try {
            Path file = Paths.get(XMLhandler.getName());
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
        lock.lock();
        try {
            marine.setCreationDate(new java.util.Date());
            marine.setId(dbManager.getNextId());
            if (dbManager.addSpaceMarine(marine))
                myStack.add(marine);
            // this.myStack.add(marine);
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            lock.unlock();
        }
    }

    public Long nextId() {
        try {
            return dbManager.getNextId();
        } catch (SQLException e) {
            e.printStackTrace();
            return null;
        }
    }

    public DatabaseManager getDataBaseManager() {
        return dbManager;
    }
}