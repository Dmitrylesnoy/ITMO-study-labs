import ru.ifmo.se.pokemon.*;
import pokemons.*;

public class Main {
    public static void main(String[] args) {
        Battle b = new Battle();
        Pokemon p1 = new Pokemon("Чужой", 1);
        Pokemon p2 = new Pokemon("Хищник", 1);
        Pokemon p3 = new Cobalion("Перелошадь", 1);
        b.addAlly(p1);
        b.addFoe(p2);
        b.addFoe(p3);
        b.go();
    }
}
