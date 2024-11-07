package lab.itmo.pokemons.moves.physical;

import ru.ifmo.se.pokemon.*;

// Has a 30% chance to poison the target.

public class PoisonJab extends PhysicalMove {
    private static final int POWER = 80;
    private static final int ACCURACY = 100;

    public PoisonJab() {
        super(Type.POISON, POWER, ACCURACY);
    }

    @Override
    protected void applyOppEffects(Pokemon p) {
        Effect e = new Effect().chance(0.3).condition(Status.POISON);
        p.addEffect(e);
    }

    protected String describe() {
        return "Использует Poison Jab";
    }
}
