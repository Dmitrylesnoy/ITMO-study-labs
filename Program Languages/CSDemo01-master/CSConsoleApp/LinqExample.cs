using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSConsoleApp
{
    internal class LinqExample
    {
        public delegate bool NumberPredicate(int number);

        public static bool IsOdd(int number)
        {
            return number % 2 == 0;
        }
        public static void Run()
        {
            List<int> numbers = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            
            //var query = from n in numbers
            //            where n % 2 == 0
            //            select n * n;

            // Use LINQ to filter even numbers and project their squares
            var evenSquares = numbers
                .Where(n => n % 2 == 0) // Filter even numbers
                .Select(n => n * n);    // Project their squares

            Console.WriteLine("Even squares:");
            foreach (var square in evenSquares)
            {
                Console.WriteLine(square);
            }
        }
    }
}
