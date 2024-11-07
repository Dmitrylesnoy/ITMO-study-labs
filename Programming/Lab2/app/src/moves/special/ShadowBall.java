package moves.special;

import ru.ifmo.se.pokemon.*;

// Shadow Ball deals damage and has a 20% chance
// of lowering the target's Special Defense by one stage.

public class ShadowBall extends SpecialMove {
    private static final int POWER = 80;
    private static final int ACCURACY = 100;

    public ShadowBall() {
        super(Type.FIRE, POWER, ACCURACY);
    }

    @Override
    protected void applyOppEffects(Pokemon p) {
        Effect e = new Effect().chance(0.2).stat(Stat.SPECIAL_DEFENSE, -1);
        p.addEffect(e);
    }

    protected String describe() {
        return "Использует Shadow ball";
    }
}
