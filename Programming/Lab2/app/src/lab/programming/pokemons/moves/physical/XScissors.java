package lab.programming.pokemons.moves.physical;

import ru.ifmo.se.pokemon.*;

// Inflicts regular damage with no additional effect.

public class XScissors extends PhysicalMove {
    private static final double POWER = 80;
    private static final double ACCURACY = 100;

    public XScissors() {
        super(Type.BUG, POWER, ACCURACY);
    }

    protected String describe() {
        return "Использует X-Scissor";
    }
}
