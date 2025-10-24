using System;

public class Frying : CookingMethod
{
    public Frying() : base("Frying")
    {
    }

    public override bool IsEdible(Vegetable vegetable, TimeSpan cookingTime)
    {
        return cookingTime >= vegetable.CookingTime;
    }
}