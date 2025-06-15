package lab8.client.controllers;

import java.net.URL;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.util.Duration;
import lab8.client.utils.Handler;
import lab8.shared.model.Coordinates;
import lab8.shared.model.SpaceMarine;

public class CardsController extends ToolbarController {

    @FXML
    private Canvas canvas;
    @FXML
    private Button reloadButton;
    @FXML
    private Button animationButton;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();
    private Timeline animationTimeline;
    private Timeline dataUpdateTimeline;
    private boolean animationRunning = true;
    private Map<Integer, Color> creatorColorMap = new HashMap<>();
    private double animationPhase = 0;
    private final SpaceMarineAnimator animator = new SpaceMarineAnimator();

    @FXML
    public void initialize(URL location,ResourceBundle resources) {
        super.initialize(location, resources);
        setupCanvas();
        setupAnimationTimeline();
        setupDataUpdateTimeline();
        canvas.setOnMouseClicked(this::handleCanvasClick);
    }

    private void setupCanvas() {
        // Clear canvas and set up space background
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.setFill(Color.BLACK); // Space background
        gc.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());
    }

    private void setupAnimationTimeline() {
        animationTimeline = new Timeline(
                new KeyFrame(Duration.millis(16), event -> { // ~60 FPS
                    if (animationRunning) {
                        animationPhase += 0.05; // Adjust for animation speed
                        renderSpaceMarines();
                    }
                }));
        animationTimeline.setCycleCount(Animation.INDEFINITE);
        animationTimeline.play();
    }
    
    private void setupDataUpdateTimeline() {
        dataUpdateTimeline = new Timeline(
                new KeyFrame(Duration.seconds(5), event -> Platform.runLater(() -> {
                    loadData();
                })));
        dataUpdateTimeline.setCycleCount(Timeline.INDEFINITE);
        dataUpdateTimeline.play();
    }

    private void loadData() {
        try {
            Stack<SpaceMarine> marineQueue = Handler.getInstance().getCollection();
            if (marineQueue == null || marineQueue.isEmpty()) {
                showAlert(Alert.AlertType.WARNING, "Warning", "No data from server");
                return;
            }
            marineData.setAll(marineQueue);
            assignColorsByCreator();
            renderSpaceMarines();
        } catch (Exception e) {
            marineData.clear();
            showAlert(Alert.AlertType.ERROR, "Error", "Failed to load data: " + e.getMessage());
        }
    }

    private void assignColorsByCreator() {
        // Get unique creator IDs
        Set<Integer> creatorIds = marineData.stream()
                .map(SpaceMarine::getCreatorId)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        // Assign a unique color to each creator ID
        creatorColorMap.clear();
        List<Color> colors = Arrays.asList(
                Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW, Color.CYAN,
                Color.MAGENTA, Color.ORANGE, Color.PURPLE, Color.LIME, Color.PINK);
        int colorIndex = 0;
        for (Integer creatorId : creatorIds) {
            creatorColorMap.put(creatorId, colors.get(colorIndex % colors.size()));
            colorIndex++;
        }
    }

    private void renderSpaceMarines() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        // Clear canvas
        gc.setFill(Color.BLACK);
        gc.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());

        // Find min and max coordinates for scaling
        double minX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 0)
                .min().orElse(0);
        double maxX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 1)
                .max().orElse(1);
        double minY = marineData.stream()
                .mapToDouble(
                        m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY()
                                : 0)
                .min().orElse(0);
        double maxY = marineData.stream()
                .mapToDouble(
                        m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY()
                                : 1)
                .max().orElse(1);

        // Avoid division by zero
        double xRange = maxX - minX != 0 ? maxX - minX : 1;
        double yRange = maxY - minY != 0 ? maxY - minY : 1;

        // Scale coordinates to canvas size with padding
        double padding = 50;
        double canvasWidth = canvas.getWidth() - 2 * padding;
        double canvasHeight = canvas.getHeight() - 2 * padding;

        for (SpaceMarine marine : marineData) {
            Coordinates coords = marine.getCoordinates();
            if (coords == null)
                continue;

            // Scale coordinates
            double x = padding + (coords.getX() - minX) / xRange * canvasWidth;
            double y = padding + (coords.getY() != null ? (coords.getY() - minY) / yRange * canvasHeight : 0);

            // Get color based on creatorId
            Color color = creatorColorMap.getOrDefault(marine.getCreatorId(), Color.GRAY);

            // Draw spaceship with appropriate animation
            drawSpaceship(gc, x, y, color, marine.getId(), animationPhase, marine.getHealth());
        }
    }

    private void drawSpaceship(GraphicsContext gc, double x, double y, Color color, Long id, double phase,
            Double health) {
        gc.setFill(color);
        gc.setStroke(color);
        gc.setLineWidth(2);

        // Compute spaceship position using animator
        double[] position = animator.computePosition(x, y, health, phase);
        double drawX = position[0];
        double drawY = position[1];

        // Save graphics context state
        gc.save();

        // Translate to spaceship position
        gc.translate(drawX, drawY);

        // Spaceship body (triangle)
        double size = 20;
        gc.beginPath();
        gc.moveTo(0, -size); // Top
        gc.lineTo(-size / 2, size); // Bottom left
        gc.lineTo(size / 2, size); // Bottom right
        gc.closePath();
        gc.fill();
        gc.stroke();

        // Pulsating thruster (flame at the bottom)
        double flameSize = animator.computeThrusterSize(size, phase);
        gc.setFill(color.deriveColor(0, 1, 1, 0.5));
        gc.beginPath();
        gc.moveTo(0, size);
        gc.lineTo(-flameSize / 4, size + flameSize);
        gc.lineTo(flameSize / 4, size + flameSize);
        gc.closePath();
        gc.fill();

        // Restore graphics context
        gc.restore();

        // Draw ID label at original position (not orbiting)
        gc.setFill(color);
        gc.setFont(javafx.scene.text.Font.font("Arial", 12));
        gc.fillText("ID: " + id, x - 20, y - size - 5);
    }

    private void handleCanvasClick(MouseEvent event) {
        if (event.getClickCount() == 2) {
            double clickX = event.getX();
            double clickY = event.getY();

            // Find clicked SpaceMarine
            SpaceMarine clickedMarine = findClickedMarine(clickX, clickY);
            if (clickedMarine != null) {
                openEditWindow(clickedMarine);
            }
        }
    }

    private SpaceMarine findClickedMarine(double clickX, double clickY) {
        double minX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 0)
                .min().orElse(0);
        double maxX = marineData.stream()
                .mapToDouble(m -> m.getCoordinates() != null ? m.getCoordinates().getX() : 1)
                .max().orElse(1);
        double minY = marineData.stream()
                .mapToDouble(
                        m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY()
                                : 0)
                .min().orElse(0);
        double maxY = marineData.stream()
                .mapToDouble(
                        m -> m.getCoordinates() != null && m.getCoordinates().getY() != null ? m.getCoordinates().getY()
                                : 1)
                .max().orElse(1);

        double padding = 50;
        double canvasWidth = canvas.getWidth() - 2 * padding;
        double canvasHeight = canvas.getHeight() - 2 * padding;
        double xRange = maxX - minX != 0 ? maxX - minX : 1;
        double yRange = maxY - minY != 0 ? maxY - minY : 1;

        for (SpaceMarine marine : marineData) {
            Coordinates coords = marine.getCoordinates();
            if (coords == null)
                continue;

            double x = padding + (coords.getX() - minX) / xRange * canvasWidth;
            double y = padding + (coords.getY() != null ? (coords.getY() - minY) / yRange * canvasHeight : 0);
            double size = 20;

            // Calculate current position for orbiting ships
            double[] position = animator.computePosition(x, y, marine.getHealth(), animationPhase);
            double drawX = position[0];
            double drawY = position[1];

            // Check if click is within the spaceship's bounding box
            if (clickX >= drawX - size / 2 && clickX <= drawX + size / 2 &&
                    clickY >= drawY - size && clickY <= drawY + size + size * 0.8) {
                return marine;
            }
        }
        return null;
    }

    private void openEditWindow(SpaceMarine marine) {
        // Placeholder for edit window
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Edit SpaceMarine");
        alert.setHeaderText("SpaceMarine Details");
        alert.setContentText(marine.toString());
        alert.showAndWait();
        // TODO: Replace with FXML-based edit window if needed
    }

    @FXML
    private void refreshData() {
        loadData();
    }

    @FXML
    private void animation() {
        animationRunning = !animationRunning;
        animationButton.setText(animationRunning ? "Pause Animation" : "Resume Animation");
    }
}