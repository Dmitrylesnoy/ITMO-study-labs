using System.Collections.Generic;

public class Recipe
{
    public string Name { get; set; }
    public List<Vegetable> Ingredients { get; private set; } = new List<Vegetable>();
    public CookingMethod CookingMethod { get; set; }

    public Recipe(string name, CookingMethod cookingMethod)
    {
        Name = name;
        CookingMethod = cookingMethod;
    }

    public void AddIngredient(Vegetable vegetable)
    {
        Ingredients.Add(vegetable);
    }
}