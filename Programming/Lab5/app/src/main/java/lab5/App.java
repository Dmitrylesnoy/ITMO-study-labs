package lab5;

import java.util.Stack;

import lab5.commands.*;

public class App {
    public static void main(String[] args) throws Exception {
        System.out.println("Hello, World!");

        Stack<Integer> testStack = new Stack<Integer>();
        testStack.push(5);
        testStack.push(4);
        testStack.push(3);

        Command cmd = new Show(testStack);
        cmd.execute();
    }
}
