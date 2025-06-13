package lab8.client.controllers;

import lab8.client.utils.Handler;
import lab8.shared.model.*;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.util.Duration;

import java.lang.reflect.Field;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.*;

public class TableController extends ToolbarController {


    @FXML
    private TableView<SpaceMarine> tableView;
    @FXML
    private Button reloadButton;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();
    private Timeline dataUpdateTimeline;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        super.initialize(location, resources);
        setupDynamicColumns();
        tableView.setItems(marineData);
        setupDataUpdateTimeline();
    }

    private void setupDynamicColumns() {
        tableView.getColumns().clear();
        List<Field> fields = getAllFields();

        for (Field field : fields) {
            String columnName = field.getName();
            Class<?> fieldType = field.getType();

            if (fieldType == Coordinates.class) {
                addColumn("coordinates.x", Double.class, marine -> {
                    Coordinates coords = marine.getCoordinates();
                    return coords != null ? String.valueOf(coords.getX()) : "";
                });
                addColumn("coordinates.y", Float.class, marine -> {
                    Coordinates coords = marine.getCoordinates();
                    return coords != null ? String.valueOf(coords.getY()) : "";
                });
            } else if (fieldType == Chapter.class) {
                addColumn("chapter.name", String.class, marine -> {
                    Chapter chapter = marine.getChapter();
                    return chapter != null ? chapter.getName() : "";
                });
                addColumn("chapter.world", String.class, marine -> {
                    Chapter chapter = marine.getChapter();
                    return chapter != null ? chapter.getWorld() : "";
                });
            } else {
                addColumn(columnName, fieldType, marine -> {
                    try {
                        field.setAccessible(true);
                        Object value = field.get(marine);
                        if (value == null)
                            return "";
                        if (fieldType == Date.class) {
                            return dateFormat.format((Date) value);
                        }
                        return value.toString();
                    } catch (IllegalAccessException e) {
                        return "";
                    }
                });
            }
        }

    }

    private List<Field> getAllFields() {
        List<Field> fields = new ArrayList<>();
        Class<?> currentClass = SpaceMarine.class;
        while (currentClass != null && currentClass != Object.class) {
            Collections.addAll(fields, currentClass.getDeclaredFields());
            currentClass = currentClass.getSuperclass();
        }
        fields.removeIf(field -> java.lang.reflect.Modifier.isTransient(field.getModifiers()));
        return fields;
    }

    private void addColumn(String name, Class<?> type,
            java.util.function.Function<SpaceMarine, String> valueExtractor) {
        TableColumn<SpaceMarine, String> column = new TableColumn<>(name);
        // column.setPrefWidth(name.contains(".") ? 150 : 100);
        // column.setPrefWidth((new Text(name)).getLayoutBounds().getWidth() + 20);
        column.setCellValueFactory(cellData -> new SimpleStringProperty(valueExtractor.apply(cellData.getValue())));
        tableView.getColumns().add(column);
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
        if (tableView == null) 
            return;
        try {
            Deque<SpaceMarine> marineDeque = Handler.getInstance().getCollection();
            // if (marineDeque == null || marineDeque.isEmpty()) 
                // showAlert(Alert.AlertType.WARNING, "Warning", "No data from server");
            marineData.setAll(marineDeque);
            tableView.refresh();
        } catch (Exception e) {
            marineData.clear();
            showAlert(Alert.AlertType.ERROR, "Error", "Failed to load data: " + e.getMessage());
        }
    }


    public void setSpaceMarines(List<SpaceMarine> list) {
        Platform.runLater(() -> {
            if (list != null && !list.isEmpty()) {
                marineData.setAll(list);
                if (tableView != null) 
                    tableView.refresh();
            } else {
                marineData.clear();
                showAlert(Alert.AlertType.WARNING, "Warning", "Invalid or empty data provided.");
            }
        });
    }

    @FXML
    public void refreshTable() {
        Platform.runLater(() -> {
            loadData();
        });
    }

    private void showAlert(Alert.AlertType type, String title, String message) {
        Platform.runLater(() -> {
            Alert alert = new Alert(type);
            alert.setTitle(title);
            alert.setHeaderText(null);
            alert.setContentText(message);
            alert.showAndWait();
        });
    }
}