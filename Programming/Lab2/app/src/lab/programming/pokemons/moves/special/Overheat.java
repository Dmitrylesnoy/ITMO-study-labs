package lab.programming.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

// Overheat deals damage but lowers the user's Special Attack
// by two stages after attacking.

public class Overheat extends SpecialMove {
    private static final double POWER = 130;
    private static final double ACCURACY = 90;

    public Overheat() {
        super(Type.FIRE, POWER, ACCURACY);
    }

    protected void applySelfEffects(Pokemon p) {
        p.setMod(Stat.SPECIAL_ATTACK, -2);
    }

    protected String describe() {
        return "Использует Overheat";
    }
}
