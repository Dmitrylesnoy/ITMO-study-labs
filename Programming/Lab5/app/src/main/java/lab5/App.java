package lab5;

import lab5.spacemarines.Coordinates;
import lab5.spacemarines.SpaceMarine;
import lab5.spacemarines.MeleeWeapon;
import lab5.io.JAXBhandler;
import lab5.io.XMLhandler;
import lab5.spacemarines.Chapter;
import lab5.utils.*;

import java.util.Stack;

public class App {
    
    
    public static void main(String[] args) {
        CollectionManager cm = new CollectionManager();
 
        SpaceMarine marine1 = new SpaceMarine("Marine1", new Coordinates(10), "Achievement1", MeleeWeapon.CHAIN_SWORD, new Chapter("Chapter1", "World1"));
        SpaceMarine marine2 = new SpaceMarine("Marine2", new Coordinates(30, (float) 9.5), "Achievement2", MeleeWeapon.POWER_SWORD, new Chapter("Chapter2", "World2"));

        cm.Add(marine1);
        cm.Add(marine2);

        Stack<SpaceMarine> stackMarine = new Stack();
        stackMarine.push(marine1);
        stackMarine.push(marine2);
        System.out.println("Colection pushed");

        XMLhandler xmLhandler = new XMLhandler();
        xmLhandler.writeCollection(stackMarine);

        Stack<SpaceMarine> readedStack = new Stack<>();
        readedStack.addAll(xmLhandler.readCollection());
        for (Object obj : readedStack) {
            SpaceMarine mar = (SpaceMarine)obj;
            System.out.println(mar.toString());
        }
    }
}
