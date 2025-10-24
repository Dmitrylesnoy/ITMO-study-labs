using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSConsoleApp
{
    public sealed class Wizard : Hero
    {
        public Wizard() : base("Wizard", 100, 40, 5)
        {

        }

        public override void ApplySpecialAbility(params Hero[] heroes)
        {
            var random = new Random();
            foreach (var hero in heroes)
            {
                int healAmount = random.Next(15, 26); // Heal between 15 and 25 HP
                hero.Heal(healAmount);
                Console.WriteLine($"{Name} heals {hero.Name} for {healAmount} HP! {hero.Name}'s current HP: {hero.HP}");
            }
        }

    }
}
