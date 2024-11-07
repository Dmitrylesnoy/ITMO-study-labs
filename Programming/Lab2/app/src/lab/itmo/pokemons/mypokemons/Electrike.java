package lab.itmo.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.itmo.pokemons.moves.status.Rest;
import lab.itmo.pokemons.moves.status.Confide;
import lab.itmo.pokemons.moves.physical.Facade;

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
