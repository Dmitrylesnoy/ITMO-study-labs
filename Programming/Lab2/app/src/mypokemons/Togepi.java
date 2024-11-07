package mypokemons;

import ru.ifmo.se.pokemon.*;
import moves.special.FireBlast;
import moves.special.ShadowBall;

public class Togepi extends Pokemon {
    private final int HP = 35;
    private final int ATTACK = 20;
    private final int DEFENSE = 65;
    private final int SPECIAL_ATTACK = 40;
    private final int SPECIAL_DEFENSE = 65;
    private final int SPEED = 20;

    public Togepi(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.FAIRY);
        setMove(new FireBlast(), new ShadowBall());
    }
}
