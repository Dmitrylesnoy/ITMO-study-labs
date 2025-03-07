package lab5.system.utils;

import java.util.Stack;

import lab5.system.io.xml.XMLhandler;
import lab5.system.model.SpaceMarine;

public class CollectionManager {
    private Stack<SpaceMarine> myStack = new Stack<>();
    private static CollectionManager instance;

    public CollectionManager() {
        CollectionManager.instance = this;
    }

    public static CollectionManager getInstance() {
        return instance == null ? new CollectionManager() : instance;
    }

    public Stack<SpaceMarine> getCollection() {
        return myStack;
    }

    public void setCollection(Stack<SpaceMarine> newStack) {
        this.myStack = newStack;
        
    }

    public void save() {
        XMLhandler.writeCollection(this.myStack);
    }

    public void load() {
        this.myStack = (Stack) XMLhandler.readCollection();
    }

    public void Add(SpaceMarine marine) {
        this.myStack.add(marine);
    }
}
