package lab5.utils;

import java.util.Stack;

import lab5.spacemarines.SpaceMarine;

public class CollectionManager {
    private Stack<SpaceMarine> myStack;

    public Stack<SpaceMarine> getCollection() {
        return myStack;
    }

    public void setCollection(Stack<SpaceMarine> newStack) {
        this.myStack = newStack;
    }

    public void Add(SpaceMarine marine) {
        myStack.push(marine);
    }
}

