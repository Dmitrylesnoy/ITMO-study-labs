import ru.ifmo.se.pokemon.*;
import mypokemons.*;

public class Main {
    public static void main(String[] args) {
        Battle b = new Battle();

        // + Иерархия всех классов UML

        // Программа должна быть размещена в пакетах по типу org.itmo.lab2 (доменное
        // имя, название компании, название проекта)

        // перепроверка

        Pokemon p1 = new Cobalion("Голубой конь", 1);
        Pokemon p2 = new Electrike("Бобик", 1);

        b.addAlly(p1);
        b.addFoe(p2);
        b.go();
    }
}
