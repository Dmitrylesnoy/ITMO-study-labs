using System;

public class Potato : Vegetable
{
    public Potato(string name, int calories, TimeSpan cookingTime) : base(name, calories, cookingTime)
    {
    }

    public override string GetDescription()
    {
        return $"Potato: {Name}. Calories: {Calories}, Cooking Time: {CookingTime.TotalMinutes} minutes.";
    }
}