using System;

public class RecipeNotFoundException : Exception
{   public RecipeNotFoundException(string message) : base(message)
    {
    }
}