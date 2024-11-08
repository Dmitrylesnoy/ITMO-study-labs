package lab.programming.pokemons.moves.special;

import ru.ifmo.se.pokemon.*;

/*
 * Magical Leaf deals damage and ignores changes to the Accuracy and Evasion stats.
 * However, it will not hit Pokémon during the invulnerable stage of
 * Bounce, Dig, Dive, Fly, Phantom Force, Shadow Force or Sky Drop.
 */

public class MagicalLeaf extends SpecialMove {
    private static final double POWER = 60;
    private static final double ACCURACY = 100;

    public MagicalLeaf() {
        super(Type.GRASS, POWER, ACCURACY);
    }

    @Override
    protected void applyOppEffects(Pokemon p) {
        Effect e = new Effect().attack(1);
        p.addEffect(e);
    }

    protected String describe() {
        return "Использует Magical Leaf";
    }
}
