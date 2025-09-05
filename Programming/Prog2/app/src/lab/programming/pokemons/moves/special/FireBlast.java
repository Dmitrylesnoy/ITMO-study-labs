package lab.programming.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

// Fire Blast deals damage and has a 10% chance of burning the target.

public class FireBlast extends SpecialMove {
    private static final double POWER = 110;
    private static final double ACCURACY = 85;

    public FireBlast() {
        super(Type.FIRE, POWER, ACCURACY);
    }

    @Override
    protected void applyOppEffects(Pokemon p) {
        Effect e = new Effect().chance(0.1);
        e.burn(p);
    }

    protected String describe() {
        return "Использует Fire Blast";
    }
}
