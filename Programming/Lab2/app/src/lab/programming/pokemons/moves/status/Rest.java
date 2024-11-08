package lab.programming.pokemons.moves.status;

import ru.ifmo.se.pokemon.*;

// User sleeps for 2 turns, but user is fully healed.

public class Rest extends StatusMove {
    private static final double POWER = 0;
    private static final double ACCURACY = 100;

    public Rest() {
        super(Type.PSYCHIC, POWER, ACCURACY);
    }

    @Override
    protected void applySelfEffects(Pokemon p) {
        p.restore();
        Effect e = new Effect().turns(2);
        e.sleep(p);
    }

    protected String describe() {
        return "Использует Rest";
    }
}
