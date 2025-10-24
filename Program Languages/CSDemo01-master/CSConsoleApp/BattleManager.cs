using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSConsoleApp
{
    public class BattleManager
    {

        private readonly List<Hero> _heroes = new();
        public BattleManager(int heroCount)
        {
            if (heroCount <= 0)
                throw new ArgumentException("Hero count must be greater than zero.", nameof(heroCount));

            for (int i = 0; i < heroCount; i++)
            {
                _heroes.Add(new Knight());
                _heroes.Add(new Wizard());
            }
        }

        public void StartBattle()
        {
            var random = new Random();
            while (_heroes.Count(h => h.HP > 0) > 1)
            {
                var attacker = _heroes.Where(h => h.HP > 0).OrderBy(_ => random.Next()).First();
                var target = _heroes.Where(h => h.HP > 0 && h != attacker).OrderBy(_ => random.Next()).FirstOrDefault();

                if (target != null)
                {
                    attacker.Attack(target);
                }
            }

            var winner = _heroes.FirstOrDefault(h => h.HP > 0);
            if (winner != null)
            {
                Console.WriteLine($"{winner.Name} is the last hero standing with {winner.HP} HP left!");
            }
            else
            {
                Console.WriteLine("All heroes have fallen!");
            }
        }
    }
}
