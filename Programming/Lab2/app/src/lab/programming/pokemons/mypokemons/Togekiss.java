package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.mypokemons.Togetic;
import lab.programming.pokemons.moves.special.DazzlingGleam;

public final class Togekiss extends Togetic {
    private final double HP = 85;
    private final double ATTACK = 50;
    private final double DEFENSE = 95;
    private final double SPECIAL_ATTACK = 120;
    private final double SPECIAL_DEFENSE = 115;
    private final double SPEED = 80;

    public Togekiss(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addMove(new DazzlingGleam());
    }
}
