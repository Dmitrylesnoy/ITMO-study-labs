package lab5;

import java.nio.charset.StandardCharsets;
import java.util.Scanner;
import java.util.Stack;

import javax.net.ssl.StandardConstants;

import lab5.system.io.Console.StdConsole;
import lab5.system.model.Chapter;
import lab5.system.model.Coordinates;
import lab5.system.model.MeleeWeapon;
import lab5.system.model.SpaceMarine;
import lab5.system.utils.CollectionManager;

public class App {

    public static void main(String[] args) {
        CollectionManager cm = new CollectionManager();

        SpaceMarine marine1 = new SpaceMarine("Marine1", new Coordinates(10), "Achievement1", MeleeWeapon.CHAIN_SWORD,
                new Chapter("Chapter1", "World1"));
        SpaceMarine marine2 = new SpaceMarine("Marine2", new Coordinates(30, (float) 9.5), Double.valueOf(156), (Boolean) true, "Achievement2",
                MeleeWeapon.POWER_SWORD, new Chapter("Chapter2", "World2"));

        cm.Add(marine1);
        cm.Add(marine2);

        Stack<SpaceMarine> stackMarine = new Stack();
        stackMarine.push(marine1);
        stackMarine.push(marine2);
        
        

        cm.save();
        StdConsole.writeln(cm.getCollection().toString());
    }
}
