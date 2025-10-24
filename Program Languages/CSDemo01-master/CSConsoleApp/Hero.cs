using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSConsoleApp
{
    public abstract class Hero
    {
        public Hero(string heroName, int hp, int attackPower, int defense)
        {
            if (string.IsNullOrWhiteSpace(heroName))
                throw new ArgumentException("Name cannot be null or empty.", nameof(heroName));
            var random = new Random();
            Name = heroName + $" #{random.Next(0, 1000)}";
            HP = hp;
            AttackPower = attackPower;
            Defense = defense;
        }

        /// <summary>
        /// Hero's name
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        /// Hero's health points
        /// </summary>
        public int HP { get; protected set; }

        public int AttackPower { get; set; }

        public int Defense { get; set; }

        public void Attack(Hero target)
        {
            int damage = AttackPower - target.Defense;

            if (damage < 0)
                damage = 0;

            target.TakeDamage(damage);

            Console.WriteLine($"{Name} attacks {target.Name} for {damage} damage!");
        }

        public void TakeDamage(int damage)
        {
            HP -= damage;

            if (this is Wizard)
            {
                Console.Beep(500, 100);
            }
            else if (this is Knight)
            {
                Console.Beep(300, 100);
            }


            //Console.Beep(damage * 100, 200);

            if (HP < 0) 
                HP = 0;

            Console.WriteLine($"{Name} takes {damage} damage! Remaining HP: {HP}");
        }

        public void Heal(int amount)
        {
            if (amount < 0)
                throw new ArgumentException("Heal amount cannot be negative.", nameof(amount));

            HP += amount;
            Console.WriteLine($"{Name} is healed by {amount}. Current HP: {HP}");
        }

        public abstract void ApplySpecialAbility(params Hero[] hero);
    }
}
