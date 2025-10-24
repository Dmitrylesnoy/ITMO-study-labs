using System;

public class Boiling : CookingMethod
{
    public Boiling() : base("Boiling")
    {
    }

    public override bool IsEdible(Vegetable vegetable, TimeSpan cookingTime)
    {
        return cookingTime >= vegetable.CookingTime;
    }
}