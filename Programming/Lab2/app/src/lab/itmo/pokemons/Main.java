package lab.itmo.pokemons;

import ru.ifmo.se.pokemon.*;
import lab.itmo.pokemons.mypokemons.*;

public class Main {
    public static void main(String[] args) {
        Battle b = new Battle();

        Pokemon p1 = new Cobalion("Голубой конь", 1);
        Pokemon p2 = new Electrike("Бобик", 1);
        Pokemon p3 = new Manectric("Лис", 1);
        Pokemon p4 = new Togepi("Кругляш", 1);
        Pokemon p5 = new Togetic("Прыгун", 1);
        Pokemon p6 = new Togekiss("Птиц", 1);

        b.addAlly(p1);
        b.addAlly(p3);
        b.addAlly(p5);
        b.addFoe(p2);
        b.addFoe(p4);
        b.addFoe(p6);
        b.go();
    }
}
