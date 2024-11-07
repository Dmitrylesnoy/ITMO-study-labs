package lab.itmo.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

// Fire Blast deals damage and has a 10% chance of burning the target.

public class FireBlast extends SpecialMove {
    private static final int POWER = 110;
    private static final int ACCURACY = 85;

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
