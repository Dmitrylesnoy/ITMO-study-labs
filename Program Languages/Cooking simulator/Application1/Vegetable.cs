using System;

public abstract class Vegetable
{
    public string Name { get; set; }
    public int Calories { get; set; }
    public TimeSpan CookingTime { get; set; }

    protected Vegetable(string name, int calories, TimeSpan cookingTime)
    {
        Name = name;
        Calories = calories;
        CookingTime = cookingTime;
    }

    public abstract string GetDescription();
}