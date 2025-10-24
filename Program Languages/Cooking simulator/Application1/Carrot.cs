using System;

public class Carrot : Vegetable
{
    public Carrot(string name, int calories, TimeSpan cookingTime) : base(name, calories, cookingTime)
    {
    }

    public override string GetDescription()
    {
        return $"Carrot: {Name}. Calories: {Calories}, Cooking Time: {CookingTime.TotalMinutes} minutes.";
    }
}