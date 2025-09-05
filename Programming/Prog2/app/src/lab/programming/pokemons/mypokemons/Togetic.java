package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.mypokemons.Togepi;
import lab.programming.pokemons.moves.special.MagicalLeaf;

public class Togetic extends Togepi {
    private final double HP = 55;
    private final double ATTACK = 40;
    private final double DEFENSE = 85;
    private final double SPECIAL_ATTACK = 80;
    private final double SPECIAL_DEFENSE = 105;
    private final double SPEED = 40;

    public Togetic(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        addType(Type.FLYING);
        addMove(new MagicalLeaf());
    }
}
