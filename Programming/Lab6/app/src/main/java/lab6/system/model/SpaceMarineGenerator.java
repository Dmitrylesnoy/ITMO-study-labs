package lab6.system.model;

import java.util.Random;

/**
 * Generates SpaceMarine objects with random characteristics using constructors
 */
public class SpaceMarineGenerator {
    private static final Random random = new Random();
    private static final String CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    private static final int MIN_NAME_LENGTH = 5;
    private static final int MAX_NAME_LENGTH = 15;
    private static final int MIN_ACHIEVEMENT_LENGTH = 10;
    private static final int MAX_ACHIEVEMENT_LENGTH = 50;
    private static final int MIN_CHAPTER_NAME_LENGTH = 5;
    private static final int MAX_CHAPTER_NAME_LENGTH = 15;
    private static final int MIN_WORLD_LENGTH = 4;
    private static final int MAX_WORLD_LENGTH = 12;

    /**
     * Generates a SpaceMarine with random characteristics
     * @return SpaceMarine with random fields
     */
    public static SpaceMarine generate() {
        if (random.nextBoolean()) {
            // Use constructor with health and loyalty
            return new SpaceMarine(
                generateRandomName(),
                generateRandomCoordinates(),
                generateRandomHealth(),
                generateRandomLoyal(),
                generateRandomAchievements(),
                generateRandomMeleeWeapon(),
                generateRandomChapter()
            );
        } else {
            return new SpaceMarine(
                generateRandomName(),
                generateRandomCoordinates(),
                generateRandomAchievements(),
                generateRandomMeleeWeapon(),
                generateRandomChapter()
            );
        }
    }

    private static String generateRandomName() {
        int length = MIN_NAME_LENGTH + random.nextInt(MAX_NAME_LENGTH - MIN_NAME_LENGTH + 1);
        return generateRandomString(length);
    }

    private static Coordinates generateRandomCoordinates() {
        return new Coordinates(
            random.nextDouble() * 1000,
            (float) (random.nextDouble() * 1000)
        );
    }

    private static String generateRandomAchievements() {
        int length = MIN_ACHIEVEMENT_LENGTH + random.nextInt(MAX_ACHIEVEMENT_LENGTH - MIN_ACHIEVEMENT_LENGTH + 1);
        return generateRandomString(length);
    }

    private static Double generateRandomHealth() {
        return 1.0 + random.nextDouble() * 999.0;
    }

    private static Boolean generateRandomLoyal() {
        return random.nextBoolean();
    }

    private static MeleeWeapon generateRandomMeleeWeapon() {
        return MeleeWeapon.values()[random.nextInt(MeleeWeapon.values().length)];
    }

    private static Chapter generateRandomChapter() {
        return new Chapter(
            generateRandomChapterName(),
            random.nextBoolean() ? generateRandomWorld() : null
        );
    }

    private static String generateRandomChapterName() {
        int length = MIN_CHAPTER_NAME_LENGTH + random.nextInt(MAX_CHAPTER_NAME_LENGTH - MIN_CHAPTER_NAME_LENGTH + 1);
        return generateRandomString(length);
    }

    private static String generateRandomWorld() {
        int length = MIN_WORLD_LENGTH + random.nextInt(MAX_WORLD_LENGTH - MIN_WORLD_LENGTH + 1);
        return generateRandomString(length);
    }

    private static String generateRandomString(int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(CHARACTERS.charAt(random.nextInt(CHARACTERS.length())));
        }
        return sb.toString();
    }
}
