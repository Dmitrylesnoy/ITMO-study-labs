package lab5;

import java.io.*;
import java.io.IOException;
import java.util.Stack;

// import jakarta.xml.bind.*;

import lab5.commands.*;
import lab5.spacemarines.*;
import lab5.utils.*;

public class App {
    public static void main(String[] args) {

        SpaceMarine marine1 = new SpaceMarine("Marine 1", new Coordinates(10,(float) -10.5), 50, true, "Get alive", MeleeWeapon.CHAIN_SWORD, new Chapter("World 1","Earth"));
        // SpaceMarine marine2 = new SpaceMarine("Marine 2", new Coordinates(20,(float) -20.5), 100, true, "Get dead", MeleeWeapon.POWER_FIST, new Chapter("World 2","Mars"));
        // SpaceMarine marine3 = new SpaceMarine("Marine 3", new Coordinates(30, (float) -30.5), "Get healthed", MeleeWeapon.POWER_BLADE, new Chapter("World 3", "Jupyter"));
        
        Stack<SpaceMarine> testStack = new Stack<SpaceMarine>();
        testStack.push(marine1);
        // testStack.push(marine2);
        // testStack.push(marine3);

        // Command cmd = new Show(testStack);
        // cmd.execute();

        XMLhandler handler = new XMLhandler();
        handler.writeStackToXML(testStack);

        
    }
}
