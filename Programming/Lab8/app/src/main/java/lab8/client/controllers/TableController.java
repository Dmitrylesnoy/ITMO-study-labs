package lab8.client.controllers;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lab8.client.controllers.dialogs.EditController;
import lab8.client.controllers.dialogs.FilterController;
import lab8.client.controllers.util.DataSyncThread;
import lab8.client.controllers.util.ToolbarController;
import lab8.client.controllers.util.LocalizationManager;
import lab8.shared.model.Chapter;
import lab8.shared.model.Coordinates;
import lab8.shared.model.SpaceMarine;

import java.lang.reflect.Field;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class TableController extends ToolbarController {
    @FXML
    private TableView<SpaceMarine> tableView;
    @FXML
    private Button reloadButton;
    @FXML
    private Button addButton;
    @FXML
    private Button filterButton;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();
    private Predicate<SpaceMarine> currentFilter = marine -> true;
    private DataSyncThread dataSyncThread;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        super.initialize(location, resources);
        setupDynamicColumns();
        dataSyncThread = new DataSyncThread(marineData, this::onDataUpdated);
        tableView.setItems(marineData);
        tableView.setOnMouseClicked(event -> {
            if (event.getButton().equals(MouseButton.PRIMARY) && event.getClickCount() == 2) {
                SpaceMarine selectedMarine = tableView.getSelectionModel().getSelectedItem();
                if (selectedMarine != null) {
                    openEditWindow(selectedMarine);
                }
            }
        });
        dataSyncThread.start();

        // Bind localized texts
        reloadButton.textProperty().bind(LocalizationManager.createStringBinding("table.reload"));
        addButton.textProperty().bind(LocalizationManager.createStringBinding("table.add"));
        filterButton.textProperty().bind(LocalizationManager.createStringBinding("table.filter"));
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
        TableColumn<SpaceMarine, String> column = new TableColumn<>();
        column.textProperty().bind(LocalizationManager.createStringBinding("filter.column." + name));
        column.setCellValueFactory(cellData -> new SimpleStringProperty(valueExtractor.apply(cellData.getValue())));
        tableView.getColumns().add(column);
    }

    private void onDataUpdated() {
        List<SpaceMarine> filteredData = marineData.stream()
                .filter(currentFilter)
                .collect(Collectors.toList());
        marineData.setAll(filteredData);
        tableView.refresh();
    }

    public void setSpaceMarines(List<SpaceMarine> list) {
        if (list != null && !list.isEmpty()) {
            List<SpaceMarine> filteredData = list.stream()
                    .filter(currentFilter)
                    .collect(Collectors.toList());
            marineData.setAll(filteredData);
            tableView.refresh();
        } else {
            marineData.clear();
            showAlert(Alert.AlertType.WARNING, LocalizationManager.getString("table.warning.invalid_data"),
                    LocalizationManager.getString("table.warning.invalid_data"));
        }
    }

    public void setFilter(Predicate<SpaceMarine> filter) {
        this.currentFilter = filter != null ? filter : marine -> true;
        onDataUpdated();
    }

    @FXML
    public void resetFilter() {
        this.currentFilter = marine -> true;
        onDataUpdated();
    }

    @FXML
    public void refreshTable() {
        dataSyncThread.refreshNow();
    }

    public void openEditWindow(SpaceMarine marine) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/edit-marine.fxml"));
            Stage stage = openWindow(loader, LocalizationManager.getString("edit.title.edit"), null, null);
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setResizable(false);

            EditController controller = loader.getController();
            controller.setTable(this).setMarine(marine);

            stage.showAndWait();
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, LocalizationManager.getString("table.error.edit_failed"),
                    LocalizationManager.getString("table.error.edit_failed") + ": " + e.getMessage());
        }
    }

    @FXML
    public void addView() {
        openAddWindow();
    }

    public void openAddWindow() {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/edit-marine.fxml"));
            Stage stage = openWindow(loader, LocalizationManager.getString("edit.title.add"), null, null);
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setResizable(false);

            EditController controller = loader.getController();
            controller.setMode(true).setMarine(new SpaceMarine()).setTable(this);

            stage.showAndWait();
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, LocalizationManager.getString("table.error.edit_failed"),
                    LocalizationManager.getString("table.error.edit_failed") + ": " + e.getMessage());
        }
    }

    @FXML
    public void filterView() {
        openFilterWindow();
    }

    public void openFilterWindow() {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/filter-marine.fxml"));
            Stage stage = openWindow(loader, LocalizationManager.getString("filter.title"), null, null);
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setResizable(false);

            FilterController controller = loader.getController();
            controller.setTable(this);

            stage.showAndWait();
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, LocalizationManager.getString("table.error.filter_failed"),
                    LocalizationManager.getString("table.error.filter_failed") + ": " + e.getMessage());
        }
    }

    public void shutdown() {
        dataSyncThread.shutdown();
    }
}