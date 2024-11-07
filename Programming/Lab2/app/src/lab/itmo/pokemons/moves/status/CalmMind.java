package lab.itmo.pokemons.moves.status;

import ru.ifmo.se.pokemon.*;

// Raises the user's Special Attack and Special Defense by one stage.

public class CalmMind extends StatusMove {
    private static final int POWER = 0;
    private static final int ACCURACY = 100;

    public CalmMind() {
        super(Type.PSYCHIC, POWER, ACCURACY);
    }

    protected void applySelfEffects(Pokemon p) {
        p.setMod(Stat.SPECIAL_ATTACK, 1);
        p.setMod(Stat.SPECIAL_DEFENSE, 1);

    }

    protected String describe() {
        return "Использует Calm Mind";
    }
}
