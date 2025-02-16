package lab5.commands;

import java.util.Stack;

public class Show implements Command {
    private Stack mystack;

    public Show(Stack stack) {
        this.mystack = stack;
    }

    public void execute(){
        for (Object i : mystack) {
            System.out.println(i);
        }
    }
}
