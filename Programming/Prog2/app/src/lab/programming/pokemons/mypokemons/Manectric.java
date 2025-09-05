package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.mypokemons.Electrike;
import lab.programming.pokemons.moves.special.Overheat;

public final class Manectric extends Electrike {
    private final double HP = 70;
    private final double ATTACK = 75;
    private final double DEFENSE = 60;
    private final double SPECIAL_ATTACK = 105;
    private final double SPECIAL_DEFENSE = 60;
    private final double SPEED = 105;

    public Manectric(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addMove(new Overheat());
    }
}
