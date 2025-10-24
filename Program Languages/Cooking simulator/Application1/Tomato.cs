using System;

public class Tomato : Vegetable
{
    public Tomato(string name, int calories, TimeSpan cookingTime) : base(name, calories, cookingTime)
    {
    }

    public override string GetDescription()
    {
        return $"Tomato: {Name}. Calories: {Calories}, Cooking Time: {CookingTime.TotalMinutes} minutes.";
    }
}