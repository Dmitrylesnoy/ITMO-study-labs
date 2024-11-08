package lab.programming.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

// Dazzling Gleam deals damage and hits all adjacent opponents
//  in double/triple battles.

public class DazzlingGleam extends SpecialMove {
    private static final double POWER = 80;
    private static final double ACCURACY = 100;
    private static final int PRIORITY = 0;
    private static final int HITS = 3;

    public DazzlingGleam() {
        super(Type.GRASS, POWER, ACCURACY, PRIORITY, HITS);
    }

    protected String describe() {
        return "Использует Dazzling Gleam";
    }
}
