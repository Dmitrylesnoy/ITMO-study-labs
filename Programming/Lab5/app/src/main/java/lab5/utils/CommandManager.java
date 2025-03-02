package lab5.utils;

import lab5.commands.*;

import lab5.spacemarines.SpaceMarine;

public class CommandManager {
    private String[] cmdArgs; // Store command name and arguments
    private CollectionManager cm; // CollectionManager instance

    public CommandManager(CollectionManager cm, String[] cmdArgs) {
        this.cm = cm; // Initialize CollectionManager
        this.cmdArgs = cmdArgs; // Store command name and arguments
    }

    public void runCommand() {
        if (cmdArgs.length == 0) {
            throw new IllegalArgumentException("No command provided.");
        }

        switch (cmdArgs[0]) { // Use the first element as the command name
            case "add":
                SpaceMarine addMarine = new SpaceMarine(); // Create a new SpaceMarine object with valid parameters
                new Add(cm, addMarine).execute(); // Pass CollectionManager and SpaceMarine
                break;
            case "clear":
                new Clear(cm).execute();
                break;
            case "exit":
                new Exit().execute(); // No parameters needed
                break;
            case "load":
                new Load(cm).execute();
                break;
            case "save":
                new Save(cm).execute();
                break;
            case "show":
                new Show(cm).execute();
                break;
            case "sort":
                new Sort(cm).execute();
                break;
            case "filter_starts_with_achievements":
                if (cmdArgs.length < 2) {
                    throw new IllegalArgumentException("Missing substring argument for filter.");
                }
                String sub = cmdArgs[1];
                new FilterStartsWithAchievements(cm, sub).execute();
                break;
            case "min_by_melee_weapon":
                new MinByMeleeWeapon(cm).execute();
                break;
            case "remove_by_id":
                if (cmdArgs.length < 2) {
                    throw new IllegalArgumentException("Missing ID argument for remove.");
                }
                Long id = Long.parseLong(cmdArgs[1]);
                new RemoveByID(cm, id).execute();
                break;
            case "remove_greater":
                SpaceMarine greaterMarine = new SpaceMarine(); // Create a new SpaceMarine object with valid parameters
                new RemoveGreater(cm, greaterMarine).execute(); // Pass CollectionManager and SpaceMarine
                break;
            case "remove_lower":
                SpaceMarine lowerMarine = new SpaceMarine(); // Create a new SpaceMarine object with valid parameters
                new RemoveLower(cm, lowerMarine).execute(); // Pass CollectionManager and SpaceMarine
                break;
            case "update_id":
                SpaceMarine updateMarine = new SpaceMarine(); // Create a new SpaceMarine object with valid parameters
                new UpdateId(cm, updateMarine).execute(); // Pass CollectionManager and SpaceMarine
                break;
            case "print_unique_loyal":
                new PrintUniqueLoyal(cm).execute(); // Pass CollectionManager
                break;
            default:
                throw new IllegalArgumentException("Unknown command: " + cmdArgs[0]);
        }
    }
}
