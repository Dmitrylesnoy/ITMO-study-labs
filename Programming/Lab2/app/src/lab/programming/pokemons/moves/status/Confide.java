package lab.programming.pokemons.moves.status;

import ru.ifmo.se.pokemon.*;

// Confide lowers the target's Special Attack by one stage.

public class Confide extends StatusMove {
    private static final double POWER = 0;
    private static final double ACCURACY = 100;

    public Confide() {
        super(Type.NORMAL, POWER, ACCURACY);
    }

    protected void applyOppEffects(Pokemon p) {
        p.setMod(Stat.SPECIAL_ATTACK, -1);
    }

    protected String describe() {
        return "Использует Confide";
    }
}
