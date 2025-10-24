using System.Collections.Generic;
using System.Text.Json;
using System.IO;

public class Data
{
    public List<Vegetable> Inventory { get; private set; } = new List<Vegetable>();
    public Dictionary<string, Recipe> Recipes { get; private set; } = new Dictionary<string, Recipe>();

    public void AddVegetable(Vegetable vegetable)
    {
        Inventory.Add(vegetable);
    }

    public void AddRecipe(Recipe recipe)
    {
        Recipes[recipe.Name] = recipe;
    }
}