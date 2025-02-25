package lab5;

import lab5.spacemarines.SpaceMarine;
import lab5.spacemarines.Coordinates;
import lab5.spacemarines.Chapter;
import lab5.spacemarines.MeleeWeapon;


import lab5.utils.XMLhandler;
import lab5.commands.*;

import java.util.Stack;


public class App {
    public static void main(String[] args) {

        SpaceMarine marine1 = new SpaceMarine("Marine 1", new Coordinates(10, (float) -10.5), 50, true, "Get alive",MeleeWeapon.CHAIN_SWORD, new Chapter("World 1", "Earth"));
        SpaceMarine marine2 = new SpaceMarine("Marine 2", new Coordinates(20,(float)-20.5), 100, true, "Get dead", MeleeWeapon.POWER_FIST, new Chapter("World2","Mars"));
        SpaceMarine marine3 = new SpaceMarine("Marine 3", new Coordinates(30, (float)-30.5), "Get healthed", MeleeWeapon.POWER_BLADE, new Chapter("World 3","Jupyter"));

        Stack<SpaceMarine> stack_Marine = new Stack<SpaceMarine>();
        stack_Marine.push(marine1);
        stack_Marine.push(marine2);
        stack_Marine.push(marine3);

        XMLhandler handler = new XMLhandler();
        // Stack<Integer> stack_int = new Stack<Integer>();
        // stack_int.push(Integer.valueOf(2));
        // stack_int.push(Integer.valueOf(5));
        // System.out.println(stack_int.toString());

        handler.writeStackToXML(marine1);

        // SpaceMarine un_marine = handler.readStackFromXML();
        // System.out.print(un_marine.toString());
        // Command cmd = new Show(testStack);
        // cmd.execute();
    }
}
