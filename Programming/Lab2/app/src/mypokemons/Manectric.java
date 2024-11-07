package mypokemons;

import ru.ifmo.se.pokemon.*;
import mypokemons.Electrike;
import moves.special.Overheat;

public final class Manectric extends Electrike {
    private final int HP = 70;
    private final int ATTACK = 75;
    private final int DEFENSE = 60;
    private final int SPECIAL_ATTACK = 105;
    private final int SPECIAL_DEFENSE = 60;
    private final int SPEED = 105;

    public Manectric(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addMove(new Overheat());
    }
}
