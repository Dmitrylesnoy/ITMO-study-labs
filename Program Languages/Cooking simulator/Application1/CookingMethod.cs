using System;

public abstract class CookingMethod
{
    public string Name { get; protected set; }

    protected CookingMethod(string name)
    {
        Name = name;
    }

    public abstract bool IsEdible(Vegetable vegetable, TimeSpan cookingTime);
}