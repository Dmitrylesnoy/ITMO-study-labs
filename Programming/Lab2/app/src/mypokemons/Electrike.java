package mypokemons;

import ru.ifmo.se.pokemon.*;
import moves.status.Rest;
import moves.status.Confide;
import moves.physical.Facade;

public class Electrike extends Pokemon {
    private final int HP = 40;
    private final int ATTACK = 45;
    private final int DEFENSE = 40;
    private final int SPECIAL_ATTACK = 65;
    private final int SPECIAL_DEFENSE = 40;
    private final int SPEED = 65;

    public Electrike(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.ELECTRIC);
        setMove(new Rest(), new Confide(), new Facade());
    }
}
