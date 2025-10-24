using System;

namespace CSConsoleApp
{
    static class Program
    {
        public static void Main(string[] args)
        {
            var heroes = GetHeroes();
            foreach (var hero in heroes)
            {
                Console.WriteLine($"{hero.Name} - HP: {hero.HP}, Attack: {hero.AttackPower}, Defense: {hero.Defense}");
            }

            var battleManager = new BattleManager(5);
            battleManager.StartBattle();
            Console.ReadKey();
        }

        public static IEnumerable<Hero> GetHeroes()
        {
            var heroes = new List<Hero>();
            for (int i = 0; i < 3; i++)
            {
                heroes.Add(new Knight());
                heroes.Add(new Wizard());
            }

            return heroes;
        }
    }
}