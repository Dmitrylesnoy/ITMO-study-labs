package lab8.client.controllers.animation;

public class SpaceMarineAnimator {
    private final double orbitRadius; // Radius for orbiting animation
    private final double maxHealth;   // Max health for speed scaling
    private final double minSpeed;    // Minimum rotation speed (radians per frame)
    private final double maxSpeed;    // Maximum rotation speed (radians per frame)
    private final double thrusterBaseSize; // Base size for thruster flame
    private final double thrusterAmplitude; // Amplitude for thruster pulsation

    public SpaceMarineAnimator() {
        this.orbitRadius = 25.0;
        this.maxHealth = 1000.0;
        this.minSpeed = 0.05;
        this.maxSpeed = 1;
        this.thrusterBaseSize = 0.5;
        this.thrusterAmplitude = 0.3;
    }

    public SpaceMarineAnimator(double orbitRadius, double maxHealth, double minSpeed, double maxSpeed,
                               double thrusterBaseSize, double thrusterAmplitude) {
        this.orbitRadius = orbitRadius;
        this.maxHealth = maxHealth;
        this.minSpeed = minSpeed;
        this.maxSpeed = maxSpeed;
        this.thrusterBaseSize = thrusterBaseSize;
        this.thrusterAmplitude = thrusterAmplitude;
    }

    /**
     * Computes the spaceship's draw position based on orbiting animation.
     * @param baseX Base X coordinate (center of orbit).
     * @param baseY Base Y coordinate (center of orbit).
     * @param health SpaceMarine's health (determines orbiting speed).
     * @param animationPhase Current animation phase.
     * @return Array [drawX, drawY] with the spaceship's position.
     */
    public double[] computePosition(double baseX, double baseY, Double health, double animationPhase) {
        double drawX = baseX;
        double drawY = baseY;

        if (health != null && health > 0) {
            // Calculate orbiting position
            double speedFactor = Math.min(health / maxHealth, 1.0) * (maxSpeed - minSpeed) + minSpeed;
            drawX = baseX + orbitRadius * Math.cos(animationPhase * speedFactor);
            drawY = baseY + orbitRadius * Math.sin(animationPhase * speedFactor);
        }

        return new double[]{drawX, drawY};
    }

    /**
     * Computes the thruster flame size for the pulsating effect.
     * @param spaceshipSize Base size of the spaceship.
     * @param animationPhase Current animation phase.
     * @return Flame size for the thruster.
     */
    public double computeThrusterSize(double spaceshipSize, double animationPhase) {
        return spaceshipSize * (thrusterBaseSize + thrusterAmplitude * Math.sin(animationPhase));
    }
}