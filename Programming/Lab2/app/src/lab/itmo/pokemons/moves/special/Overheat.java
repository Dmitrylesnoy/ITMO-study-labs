package lab.itmo.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

// Overheat deals damage but lowers the user's Special Attack
// by two stages after attacking.

public class Overheat extends SpecialMove {
    private static final int POWER = 130;
    private static final int ACCURACY = 90;

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
