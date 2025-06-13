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
import java.util.logging.Logger;

public class TableController extends ToolbarController {

    private static final Logger LOGGER = Logger.getLogger(TableController.class.getName());

    @FXML
    private TableView<SpaceMarine> tableView;
    @FXML
    private Button reloadButton;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();
    private Timeline dataUpdateTimeline;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        LOGGER.info("Initializing TableController...");

        // Проверка инициализации TableView
        if (tableView == null) {
            LOGGER.severe(
                    "TableView is null. Possible causes: incorrect fx:id in FXML, wrong FXML file path, or FXML loading failure.");
            showAlert(Alert.AlertType.ERROR, "Initialization Error",
                    "TableView is not initialized. Check that fx:id='tableView' is set in table.fxml and the FXML file is loaded correctly.");
            return;
        }

        // Проверка инициализации кнопки
        if (reloadButton == null) {
            LOGGER.severe("reloadButton is null. Check fx:id='reloadButton' in FXML.");
            showAlert(Alert.AlertType.ERROR, "Initialization Error",
                    "Reload button is not initialized. Check fx:id='reloadButton' in table.fxml.");
            return;
        }

        // Вызов initialize родительского класса
        super.initialize(location, resources);

        // Настройка столбцов динамически
        setupDynamicColumns();

        // Привязка данных
        tableView.setItems(marineData);
        LOGGER.info("TableView successfully bound to marineData.");

        // Настройка периодического обновления
        setupDataUpdateTimeline();

        // Загрузка данных
        // Platform.runLater(() -> {
        //     loadData();
        //     logMarineData();
        //     LOGGER.info("Initial data load completed.");
        // });
    }

    private void setupDynamicColumns() {
        tableView.getColumns().clear();
        List<Field> fields = getAllFields();

        for (Field field : fields) {
            String columnName = field.getName();
            Class<?> fieldType = field.getType();

            // Обработка вложенных классов
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
                // Обработка простых полей
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
                        LOGGER.warning("Failed to access field " + columnName + ": " + e.getMessage());
                        return "";
                    }
                });
            }
        }

        LOGGER.info("Added " + tableView.getColumns().size() + " columns to TableView.");
    }

    private List<Field> getAllFields() {
        List<Field> fields = new ArrayList<>();
        Class<?> currentClass = SpaceMarine.class;

        // Собираем все поля, включая унаследованные
        while (currentClass != null && currentClass != Object.class) {
            Collections.addAll(fields, currentClass.getDeclaredFields());
            currentClass = currentClass.getSuperclass();
        }

        // Исключаем transient поля
        fields.removeIf(field -> java.lang.reflect.Modifier.isTransient(field.getModifiers()));
        return fields;
    }

    private void addColumn(String name, Class<?> type,
            java.util.function.Function<SpaceMarine, String> valueExtractor) {
        TableColumn<SpaceMarine, String> column = new TableColumn<>(name);
        column.setPrefWidth(name.contains(".") ? 150 : 100);
        column.setCellValueFactory(cellData -> new SimpleStringProperty(valueExtractor.apply(cellData.getValue())));
        tableView.getColumns().add(column);
    }

    private void setupDataUpdateTimeline() {
        dataUpdateTimeline = new Timeline(
                new KeyFrame(Duration.seconds(5), event -> Platform.runLater(() -> {
                    loadData();
                    logMarineData();
                })));
        dataUpdateTimeline.setCycleCount(Timeline.INDEFINITE);
        dataUpdateTimeline.play();
        LOGGER.info("Data update timeline started.");
    }

    private void loadData() {
        if (tableView == null) {
            LOGGER.severe("Cannot load data: tableView is null.");
            return;
        }

        try {
            Deque<SpaceMarine> marineDeque = Handler.getInstance().getCollection();
            if (marineDeque == null || marineDeque.isEmpty()) {
                // Тестовые данные
                marineDeque = new ArrayDeque<>();
                SpaceMarine marine1 = new SpaceMarine(
                        "TestMarine",
                        new Coordinates(1.0, 2.0f),
                        "TestAchievements",
                        null,
                        null);
                marine1.setId(1L);
                marine1.setCreationDate(new Date());
                marine1.setHealth(100.0);
                marine1.setLoyal(true);
                marine1.setCreatorId(1);

                SpaceMarine marine2 = new SpaceMarine(
                        "TestMarine2",
                        new Coordinates(3.0, 4.0f),
                        "MoreAchievements",
                        MeleeWeapon.CHAIN_SWORD,
                        new Chapter("TestChapter", "TestWorld"));
                marine2.setId(2L);
                marine2.setCreationDate(new Date());
                marine2.setHealth(200.0);
                marine2.setLoyal(false);
                marine2.setCreatorId(2);

                marineDeque.add(marine1);
                marineDeque.add(marine2);
                LOGGER.warning("No data from Handler, using test data: " + marineDeque.size() + " SpaceMarines.");
                showAlert(Alert.AlertType.WARNING, "Warning", "No data from server, displaying test data.");
            }
            marineData.setAll(marineDeque);
            LOGGER.info("Loaded " + marineDeque.size() + " SpaceMarines into table.");
            tableView.refresh();
        } catch (Exception e) {
            marineData.clear();
            showAlert(Alert.AlertType.ERROR, "Error", "Failed to load data: " + e.getMessage());
            LOGGER.severe("Failed to load data: " + e.getMessage());
        }
    }

    private void logMarineData() {
        if (marineData.isEmpty()) {
            LOGGER.info("marineData is empty.");
        } else {
            LOGGER.info("marineData contains " + marineData.size() + " items:");
            for (SpaceMarine marine : marineData) {
                StringBuilder log = new StringBuilder("SpaceMarine: ");
                for (Field field : getAllFields()) {
                    try {
                        field.setAccessible(true);
                        Object value = field.get(marine);
                        log.append(field.getName()).append("=").append(value != null ? value.toString() : "null")
                                .append(", ");
                    } catch (IllegalAccessException e) {
                        log.append(field.getName()).append("=error, ");
                    }
                }
                Coordinates coords = marine.getCoordinates();
                log.append("coordinates.x=").append(coords != null ? coords.getX() : "null").append(", ");
                log.append("coordinates.y=").append(coords != null ? coords.getY() : "null").append(", ");
                Chapter chapter = marine.getChapter();
                log.append("chapter.name=").append(chapter != null ? chapter.getName() : "null").append(", ");
                log.append("chapter.world=").append(chapter != null ? chapter.getWorld() : "null");
                LOGGER.info(log.toString());
            }
        }
    }

    public void setSpaceMarines(List<SpaceMarine> list) {
        Platform.runLater(() -> {
            if (list != null && !list.isEmpty()) {
                marineData.setAll(list);
                if (tableView != null) {
                    tableView.refresh();
                }
                LOGGER.info("Set " + list.size() + " SpaceMarines in table.");
                logMarineData();
            } else {
                marineData.clear();
                showAlert(Alert.AlertType.WARNING, "Warning", "Invalid or empty data provided.");
                LOGGER.warning("Provided list is null or empty.");
            }
        });
    }

    @FXML
    public void refreshTable() {
        Platform.runLater(() -> {
            loadData();
            logMarineData();
            LOGGER.info("Table refreshed manually.");
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