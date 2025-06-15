package lab8.client.controllers;

import javafx.animation.Timeline;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.input.MouseEvent;
import lab8.client.controllers.animation.SpaceMarineAnimator;
import lab8.client.controllers.animation.SpaceMarineRenderer;
import lab8.client.controllers.util.DataSyncThread;
import lab8.client.controllers.util.ToolbarController;
import lab8.shared.model.SpaceMarine;

import java.net.URL;
import java.util.ResourceBundle;

public class CardsController extends ToolbarController {
    @FXML private Canvas canvas;
    @FXML
    private Button reloadButton;
    @FXML
    private Button animationButton;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();
    private boolean animationRunning = true;
    private Timeline animationTimeline;
    private DataSyncThread dataSyncThread;
    private SpaceMarineRenderer renderer;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        super.initialize(location, resources);
        renderer = new SpaceMarineRenderer(canvas, marineData);
        dataSyncThread = new DataSyncThread(marineData, renderer::updateColorsAndRender);
        setupAnimationTimeline();
        canvas.setOnMouseClicked(this::handleCanvasClick);
        dataSyncThread.start();
    }

    private void setupAnimationTimeline() {
        animationTimeline = renderer.createAnimationTimeline(() -> animationRunning);
        animationTimeline.play();
    }

    private void handleCanvasClick(MouseEvent event) {
        if (event.getClickCount() == 2) {
            SpaceMarine marine = renderer.findMarineAt(event.getX(), event.getY());
            if (marine != null) {
                openEditWindow(marine);
            }
        }
    }

    private void openEditWindow(SpaceMarine marine) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Edit SpaceMarine");
        alert.setContentText(marine.toString());
        alert.showAndWait();
    }

    @FXML
    private void refreshData() {
        dataSyncThread.refreshNow();
    }

    @FXML
    private void animation() {
        animationRunning = !animationRunning;
        animationButton.setText(animationRunning ? "Pause Animation" : "Resume Animation");
    }

    public void shutdown() {
        dataSyncThread.shutdown();
        animationTimeline.stop();
    }
}