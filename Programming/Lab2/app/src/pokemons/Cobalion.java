package pokemons;

import ru.ifmo.se.pokemon.*;

public class Cobalion extends Pokemon{
    public Cobalion(String name, int level){
        super(name, level);
        setStats(91, 90, 129, 90, 72, 108);
        setType(Type.STEEL, Type.FIGHTING);
        // setMove();
    }
}
