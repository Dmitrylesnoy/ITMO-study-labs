package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.moves.status.Rest;
import lab.programming.pokemons.moves.status.Confide;
import lab.programming.pokemons.moves.physical.Facade;

public class Electrike extends Pokemon {
    private final double HP = 40;
    private final double ATTACK = 45;
    private final double DEFENSE = 40;
    private final double SPECIAL_ATTACK = 65;
    private final double SPECIAL_DEFENSE = 40;
    private final double SPEED = 65;

    public Electrike(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.ELECTRIC);
        setMove(new Rest(), new Confide(), new Facade());
    }
}
