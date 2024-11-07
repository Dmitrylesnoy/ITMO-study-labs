package lab.itmo.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.itmo.pokemons.moves.status.Rest;
import lab.itmo.pokemons.moves.status.CalmMind;
import lab.itmo.pokemons.moves.physical.XScissors;
import lab.itmo.pokemons.moves.physical.PoisonJab;

public final class Cobalion extends Pokemon {
    private final int HP = 91;
    private final int ATTACK = 90;
    private final int DEFENSE = 129;
    private final int SPECIAL_ATTACK = 90;
    private final int SPECIAL_DEFENSE = 72;
    private final int SPEED = 108;

    public Cobalion(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.STEEL, Type.FIGHTING);
        setMove(new Rest(), new XScissors(), new PoisonJab(), new CalmMind());
    }
}
