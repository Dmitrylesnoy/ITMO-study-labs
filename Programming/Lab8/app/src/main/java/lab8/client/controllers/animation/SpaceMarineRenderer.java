package lab8.client.controllers.animation;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.collections.ObservableList;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.util.Duration;
import lab8.shared.model.Coordinates;
import lab8.shared.model.SpaceMarine;

public class SpaceMarineRenderer {
    private final Canvas canvas;
    private final ObservableList<SpaceMarine> marineData;
    private final SpaceMarineAnimator animator;
    private final Map<Integer, Color> creatorColorMap = new HashMap<>();
    private double minX, maxX, minY, maxY;
    private double animationPhase;

    public SpaceMarineRenderer(Canvas canvas, ObservableList<SpaceMarine> marineData) {
        this.canvas = canvas;
        this.marineData = marineData;
        this.animator = new SpaceMarineAnimator();
        clearCanvas();
    }

    public Timeline createAnimationTimeline(Supplier<Boolean> isAnimationRunning) {
        Timeline timeline = new Timeline(
                new KeyFrame(Duration.millis(16), e -> {
                    if (isAnimationRunning.get()) {
                        animationPhase += 0.05;
                        render();
                    }
                })
        );
        timeline.setCycleCount(Timeline.INDEFINITE);
        return timeline;
    }

    public void updateColorsAndRender() {
        assignColors();
        updateBounds();
        render();
    }

    private void clearCanvas() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.setFill(Color.BLACK);
        gc.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());
    }

    private void render() {
        clearCanvas();
        if (marineData.isEmpty()) return;

        GraphicsContext gc = canvas.getGraphicsContext2D();
        double padding = 50;
        double canvasWidth = canvas.getWidth() - 2 * padding;
        double canvasHeight = canvas.getHeight() - 2 * padding;
        double xRange = maxX - minX != 0 ? maxX - minX : 1;
        double yRange = maxY - minY != 0 ? maxY - minY : 1;

        for (SpaceMarine marine : marineData) {
            Coordinates coords = marine.getCoordinates();
            if (coords == null) continue;

            double x = padding + (coords.getX() - minX) / xRange * canvasWidth;
            double y = padding + (coords.getY() != null ? (coords.getY() - minY) / yRange * canvasHeight : 0);
            Color color = creatorColorMap.getOrDefault(marine.getCreatorId(), Color.GRAY);
            drawSpaceship(gc, x, y, color, marine.getId(), marine.getHealth());
        }
    }

    private void drawSpaceship(GraphicsContext gc, double x, double y, Color color, Long id, Double health) {
        gc.setFill(color);
        gc.setStroke(color);
        gc.setLineWidth(2);

        double[] position = animator.computePosition(x, y, health, animationPhase);
        double drawX = position[0], drawY = position[1];

        gc.save();
        gc.translate(drawX, drawY);

        double size = 20;
        gc.beginPath();
        gc.moveTo(0, -size);
        gc.lineTo(-size / 2, size);
        gc.lineTo(size / 2, size);
        gc.closePath();
        gc.fill();
        gc.stroke();

        double flameSize = animator.computeThrusterSize(size, animationPhase);
        gc.setFill(color.deriveColor(0, 1, 1, 0.5));
        gc.beginPath();
        gc.moveTo(0, size);
        gc.lineTo(-flameSize / 4, size + flameSize);
        gc.lineTo(flameSize / 4, size + flameSize);
        gc.closePath();
        gc.fill();

        gc.restore();

        gc.setFill(color);
        gc.setFont(javafx.scene.text.Font.font("Arial", 12));
        gc.fillText("ID: " + id, drawX - 20, drawY - size - 5);
    }

    public SpaceMarine findMarineAt(double clickX, double clickY) {
        double padding = 50;
        double canvasWidth = canvas.getWidth() - 2 * padding;
        double canvasHeight = canvas.getHeight() - 2 * padding;
        double xRange = maxX - minX != 0 ? maxX - minX : 1;
        double yRange = maxY - minY != 0 ? maxY - minY : 1;

        for (SpaceMarine marine : marineData) {
            Coordinates coords = marine.getCoordinates();
            if (coords == null) continue;

            double x = padding + (coords.getX() - minX) / xRange * canvasWidth;
            double y = padding + (coords.getY() != null ? (coords.getY() - minY) / yRange * canvasHeight : 0);
            double[] position = animator.computePosition(x, y, marine.getHealth(), animationPhase);
            double drawX = position[0], drawY = position[1];
            double size = 20;

            if (clickX >= drawX - size / 2 && clickX <= drawX + size / 2 &&
                    clickY >= drawY - size && clickY <= drawY + size + size * 0.8) {
                return marine;
            }
        }
        return null;
    }

    private void assignColors() {
        creatorColorMap.clear();
        List<Color> baseColors = List.of(
                Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW, Color.CYAN,
                Color.MAGENTA, Color.ORANGE, Color.PURPLE, Color.LIME, Color.PINK
        );
        List<Integer> creatorIds = marineData.stream()
                .map(SpaceMarine::getCreatorId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
        for (int i = 0; i < creatorIds.size(); i++) {
            int id = creatorIds.get(i);
            if (i < baseColors.size()) {
                creatorColorMap.put(id, baseColors.get(i));
            } else {
                double hue = (i * 360.0 / creatorIds.size()) % 360;
                creatorColorMap.put(id, Color.hsb(hue, 0.7, 0.9));
            }
        }
    }

    private void updateBounds() {
        minX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 0)
                .min().orElse(0);
        maxX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 1)
                .max().orElse(1);
        minY = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY() : 0)
                .min().orElse(0);
        maxY = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY() : 1)
                .max().orElse(1);
    }
}