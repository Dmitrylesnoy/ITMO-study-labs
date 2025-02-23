package lab5;

import java.util.Stack;

import lab5.spacemarines.Chapter;
import lab5.spacemarines.Coordinates;
import lab5.spacemarines.MeleeWeapon;
import lab5.spacemarines.SpaceMarine;
import lab5.utils.XMLhandler;
import java.io.IOException;
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import lab5.commands.*;
import lab5.spacemarines.*;
import lab5.utils.*;

public class App {
    public static void main(String[] args) {

        SpaceMarine marine1 = new SpaceMarine("Marine 1", new Coordinates(10, (float) -10.5), 50, true, "Get alive",MeleeWeapon.CHAIN_SWORD, new Chapter("World 1", "Earth"));
        SpaceMarine marine2 = new SpaceMarine("Marine 2", new Coordinates(20,(float)-20.5), 100, true, "Get dead", MeleeWeapon.POWER_FIST, new Chapter("World2","Mars"));
        SpaceMarine marine3 = new SpaceMarine("Marine 3", new Coordinates(30, (float)-30.5), "Get healthed", MeleeWeapon.POWER_BLADE, new Chapter("World 3","Jupyter"));

        Stack<SpaceMarine> stack_Marine = new Stack<SpaceMarine>();
        stack_Marine.push(marine1);
        stack_Marine.push(marine2);
        stack_Marine.push(marine3);

        Stack<Chapter> stack_Chapter = new Stack<Chapter>();
        stack_Chapter.push(new Chapter("TestChapter", "TestTest"));

        XMLhandler handler = new XMLhandler();
        handler.writeStackToXML(marine3);

        // Command cmd = new Show(testStack);
        // cmd.execute();
       
    }
}
