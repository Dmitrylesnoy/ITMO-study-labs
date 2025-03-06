package lab5.system.utils;

import java.util.Stack;

import lab5.system.io.file.XMLhandler;
import lab5.system.model.SpaceMarine;

public class CollectionManager {
    private Stack<SpaceMarine> myStack = new Stack<>(); // Initialize the stack

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
