package lab5;

import java.util.Stack;

import lab5.system.Handler;
import lab5.system.io.console.StdConsole;
import lab5.system.model.*;
import lab5.system.utils.CollectionManager;

public class App {

    public static void main(String[] args) {

        SpaceMarine marine1 = new SpaceMarine("Marine1", new Coordinates(10), "Achievement1", MeleeWeapon.CHAIN_SWORD,
                new Chapter("Chapter1", "World1"));
        SpaceMarine marine2 = new SpaceMarine("Marine2", new Coordinates(30, (float) 9.5), Double.valueOf(156),
                (Boolean) true, "Achievement2",
                MeleeWeapon.POWER_SWORD, new Chapter("Chapter2", "World2"));

        // CollectionManager cm = new CollectionManager();;

        // cm.Add(marine1);
        // cm.Add(marine2);

        Handler handler = new Handler(new StdConsole());
        
        while (true) {
            handler.Run();
        }
    }
}
