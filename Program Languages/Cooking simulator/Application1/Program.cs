using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        Data data = new Data();
        bool running = true;

        while (running)
        {
            DisplayMenu();
            string choice = Console.ReadLine()?.Trim();

            try
            {
                switch (choice)
                {
                    case "1":
                        AddVegetable(data);
                        break;
                    case "2":
                        CreateRecipe(data);
                        break;
                    case "3":
                        SimulateCooking(data);
                        break;
                    case "4":
                        ViewInventory(data);
                        break;
                    case "5":
                        ViewRecipes(data);
                        break;
                    case "6":
                        running = false;
                        break;
                    default:
                        Console.WriteLine("Invalid choice. Please select 1-6.");
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An error occurred: {ex.Message}");
            }
        }
    }

    static void DisplayMenu()
    {
        Console.WriteLine("\nVegetable Cooking Data Menu:");
        Console.WriteLine("1. Add Vegetable");
        Console.WriteLine("2. Create Recipe");
        Console.WriteLine("3. Simulate Cooking");
        Console.WriteLine("4. View Inventory");
        Console.WriteLine("5. View Recipes");
        Console.WriteLine("6. Exit");
        Console.Write("Choose an option: ");
    }

    static void AddVegetable(Data data)
    {
        Console.WriteLine("\nSelect vegetable type:");
        Console.WriteLine("1. Carrot");
        Console.WriteLine("2. Potato");
        Console.WriteLine("3. Tomato");
        Console.Write("Choice: ");
        string typeChoice = Console.ReadLine()?.Trim();

        Vegetable vegetable = null;
        string name;
        int calories;
        double cookingTimeMinutes;

        switch (typeChoice)
        {
            case "1":
                Console.Write("Enter carrot name: ");
                name = Console.ReadLine();
                if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name cannot be empty.");
                break;
            case "2":
                Console.Write("Enter potato name: ");
                name = Console.ReadLine();
                if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name cannot be empty.");
                break;
            case "3":
                Console.Write("Enter tomato name: ");
                name = Console.ReadLine();
                if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name cannot be empty.");
                break;
            default:
                Console.WriteLine("Invalid type.");
                return;
        }

        Console.Write("Enter calories (positive integer): ");
        if (!int.TryParse(Console.ReadLine(), out calories) || calories <= 0)
        {
            Console.WriteLine("Invalid calories. Must be positive integer.");
            return;
        }

        Console.Write("Enter cooking time in minutes (positive number): ");
        if (!double.TryParse(Console.ReadLine(), out cookingTimeMinutes) || cookingTimeMinutes <= 0)
        {
            Console.WriteLine("Invalid cooking time. Must be positive number.");
            return;
        }

        TimeSpan cookingTime = TimeSpan.FromMinutes(cookingTimeMinutes);

        switch (typeChoice)
        {
            case "1":
                vegetable = new Carrot(name, calories, cookingTime);
                break;
            case "2":
                vegetable = new Potato(name, calories, cookingTime);
                break;
            case "3":
                vegetable = new Tomato(name, calories, cookingTime);
                break;
        }

        data.AddVegetable(vegetable);
        Console.WriteLine($"Added {vegetable.Name} to inventory.");
    }

    static void CreateRecipe(Data data)
    {
        if (data.Inventory.Count == 0)
        {
            Console.WriteLine("No vegetables in inventory. Add some first.");
            return;
        }

        Console.Write("Enter recipe name: ");
        string name = Console.ReadLine();
        if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name cannot be empty.");

        Console.WriteLine("Select cooking method:");
        Console.WriteLine("1. Boiling");
        Console.WriteLine("2. Frying");
        Console.Write("Choice: ");
        string methodChoice = Console.ReadLine()?.Trim();

        CookingMethod cookingMethod;
        switch (methodChoice)
        {
            case "1":
                cookingMethod = new Boiling();
                break;
            case "2":
                cookingMethod = new Frying();
                break;
            default:
                Console.WriteLine("Invalid method.");
                return;
        }

        Recipe recipe = new Recipe(name, cookingMethod);

        Console.WriteLine("Select vegetables to add (comma-separated indices, e.g. 1,3,5):");
        for (int i = 0; i < data.Inventory.Count; i++)
        {
            Console.WriteLine($"{i + 1}. {data.Inventory[i].Name}");
        }
        Console.Write("Indices: ");
        string indicesInput = Console.ReadLine();
        var indices = indicesInput.Split(',').Select(s => s.Trim()).ToArray();

        foreach (var idxStr in indices)
        {
            if (int.TryParse(idxStr, out int idx) && idx >= 1 && idx <= data.Inventory.Count)
            {
                recipe.AddIngredient(data.Inventory[idx - 1]);
            }
            else
            {
                Console.WriteLine($"Invalid index: {idxStr}");
            }
        }

        if (recipe.Ingredients.Count == 0)
        {
            Console.WriteLine("No valid ingredients added.");
            return;
        }

        data.AddRecipe(recipe);
        Console.WriteLine($"Created recipe: {name}");
    }

    static void SimulateCooking(Data data)
    {
        if (data.Recipes.Count == 0)
        {
            Console.WriteLine("No recipes available. Create some first.");
            return;
        }

        Console.WriteLine("Available recipes:");
        foreach (var name in data.Recipes.Keys)
        {
            Console.WriteLine($"- {name}");
        }

        Console.Write("Enter recipe name: ");
        string recipeName = Console.ReadLine();
        if (!data.Recipes.TryGetValue(recipeName, out Recipe recipe))
        {
            throw new RecipeNotFoundException($"Recipe '{recipeName}' not found.");
        }

        Console.Write("Enter cooking time in minutes: ");
        if (!double.TryParse(Console.ReadLine(), out double cookingTimeMinutes) || cookingTimeMinutes < 0)
        {
            throw new InvalidDataException("Cooking time must be positive number.");
        }

        TimeSpan cookingTime = TimeSpan.FromMinutes(cookingTimeMinutes);

        Console.WriteLine($"\nSimulating cooking '{recipeName}' for {cookingTime.TotalMinutes} minutes using {recipe.CookingMethod.Name}...");

        foreach (var veg in recipe.Ingredients)
        {
            bool edible = recipe.CookingMethod.IsEdible(veg, cookingTime);
            Console.WriteLine($"{veg.Name}: {veg.GetDescription()} - {(edible ? "Edible" : "Not edible")}");
        }
    }

    static void ViewInventory(Data data)
    {
        if (data.Inventory.Count == 0)
        {
            Console.WriteLine("Inventory is empty.");
            return;
        }

        Console.WriteLine("\nInventory:");
        foreach (var veg in data.Inventory)
        {
            Console.WriteLine(veg.GetDescription());
        }
    }

    static void ViewRecipes(Data data)
    {
        if (data.Recipes.Count == 0)
        {
            Console.WriteLine("No recipes available.");
            return;
        }

        Console.WriteLine("\nRecipes:");
        foreach (var recipe in data.Recipes.Values)
        {
            Console.WriteLine($"- {recipe.Name} ({recipe.CookingMethod.Name})");
            Console.WriteLine("  Ingredients:");
            foreach (var ing in recipe.Ingredients)
            {
                Console.WriteLine($"    {ing.Name}");
            }
        }
    }
}
