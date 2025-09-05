package lab.programming.pokemons.moves.physical;

import ru.ifmo.se.pokemon.*;

// Facade deals damage, and hits with double power (140) 
// if the user is burned, poisoned or paralyzed.

public class Facade extends PhysicalMove {
    private static final double POWER = 70;
    private static final double ACCURACY = 100;

    public Facade() {
        super(Type.NORMAL, POWER, ACCURACY);
    }

    @Override
    protected void applyOppDamage(Pokemon def, double damage) {
        Status defStatus = def.getCondition();

        if ((defStatus.equals(Status.BURN)) || (defStatus.equals(Status.POISON))
                || (defStatus.equals(Status.PARALYZE))) {
            def.setMod(Stat.HP, (int) (damage * 2));
        } else {
            def.setMod(Stat.HP, (int) (damage));
        }
    }

    @Override
    protected String describe() {
        return "Использует Facade";
    }
}
