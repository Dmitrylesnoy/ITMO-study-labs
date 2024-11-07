package mypokemons;

import ru.ifmo.se.pokemon.*;
import mypokemons.Togepi;
import moves.special.MagicalLeaf;

public class Togetic extends Togepi {
    private final int HP = 55;
    private final int ATTACK = 40;
    private final int DEFENSE = 85;
    private final int SPECIAL_ATTACK = 80;
    private final int SPECIAL_DEFENSE = 105;
    private final int SPEED = 40;

    public Togetic(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addType(Type.FLYING);
        addMove(new MagicalLeaf());
    }
}
