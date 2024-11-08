package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.moves.status.Rest;
import lab.programming.pokemons.moves.status.CalmMind;
import lab.programming.pokemons.moves.physical.XScissors;
import lab.programming.pokemons.moves.physical.PoisonJab;

public final class Cobalion extends Pokemon {
    private final double HP = 91;
    private final double ATTACK = 90;
    private final double DEFENSE = 129;
    private final double SPECIAL_ATTACK = 90;
    private final double SPECIAL_DEFENSE = 72;
    private final double SPEED = 108;

    public Cobalion(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.STEEL, Type.FIGHTING);
        setMove(new Rest(), new XScissors(), new PoisonJab(), new CalmMind());
    }
}
