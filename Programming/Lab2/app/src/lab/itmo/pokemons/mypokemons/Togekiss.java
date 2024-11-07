package lab.itmo.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.itmo.pokemons.mypokemons.Togetic;
import lab.itmo.pokemons.moves.special.DazzlingGleam;

public final class Togekiss extends Togetic {
    private final int HP = 85;
    private final int ATTACK = 50;
    private final int DEFENSE = 95;
    private final int SPECIAL_ATTACK = 120;
    private final int SPECIAL_DEFENSE = 115;
    private final int SPEED = 80;

    public Togekiss(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addMove(new DazzlingGleam());
    }
}
