package lab.programming.pokemons.mypokemons;

import ru.ifmo.se.pokemon.*;
import lab.programming.pokemons.moves.special.FireBlast;
import lab.programming.pokemons.moves.special.ShadowBall;

public class Togepi extends Pokemon {
    private final double HP = 35;
    private final double ATTACK = 20;
    private final double DEFENSE = 65;
    private final double SPECIAL_ATTACK = 40;
    private final double SPECIAL_DEFENSE = 65;
    private final double SPEED = 20;

    public Togepi(String name, int level) {
        super(name, level);
        setStats(HP, ATTACK, DEFENSE, SPECIAL_ATTACK, SPECIAL_DEFENSE, SPEED);
        setType(Type.FAIRY);
        setMove(new FireBlast(), new ShadowBall());
    }
}
