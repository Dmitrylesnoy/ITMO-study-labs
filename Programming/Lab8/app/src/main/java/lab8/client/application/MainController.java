package lab8.client.application;

import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.fxml.FXML;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.paint.Color;
import lab8.shared.messages.User;
import lab8.shared.model.SpaceMarine;

public class MainController {
    @FXML
    private MenuBar menuBar;
    @FXML
    private Menu languageMenu;
    @FXML
    private Label currentUserLabel;
    @FXML
    private Button logoutButton;
    @FXML
    private TextField filterField;
    @FXML
    private Button applyFilterButton;
    @FXML
    private Button resetFilterButton;
    @FXML
    private TableView<SpaceMarine> collectionTable;
    @FXML
    private TableColumn<SpaceMarine, Integer> idColumn;
    @FXML
    private TableColumn<SpaceMarine, String> nameColumn;
    @FXML
    private TableColumn<SpaceMarine, String> ownerColumn;
    @FXML
    private TableColumn<SpaceMarine, Double> xColumn;
    @FXML
    private TableColumn<SpaceMarine, Double> yColumn;
    @FXML
    private TableColumn<SpaceMarine, String> creationDateColumn;
    @FXML
    private Canvas visualizationCanvas;
    @FXML
    private Button addButton;
    @FXML
    private Button editButton;
    @FXML
    private Button removeButton;
    @FXML
    private TextField commandField;
    @FXML
    private Button executeButton;
    @FXML
    private Label statusLabel;

    private ObservableList<SpaceMarine> collectionData = FXCollections.observableArrayList();
    private FilteredList<SpaceMarine> filteredData;
    private SortedList<SpaceMarine> sortedData;

    private ClientApp clientApp;
    private User currentUser;

    public void setClientApp(ClientApp clientApp) {
        this.clientApp = clientApp;
    }

    public void setCurrentUser(User user) {
        this.currentUser = user;
        currentUserLabel.setText(user.login());
    }

    @FXML
    private void initialize() {
        setupLanguageMenu();
        updateTexts();
        // Настройка таблицы
        idColumn.setCellValueFactory(new PropertyValueFactory<>("id"));
        nameColumn.setCellValueFactory(new PropertyValueFactory<>("name"));
        ownerColumn.setCellValueFactory(new PropertyValueFactory<>("creator_id"));
        xColumn.setCellValueFactory(new PropertyValueFactory<>("x"));
        yColumn.setCellValueFactory(new PropertyValueFactory<>("y"));
        creationDateColumn.setCellValueFactory(new PropertyValueFactory<>("creationDate"));

        // Настройка фильтрации и сортировки
        filteredData = new FilteredList<>(collectionData, p -> true);
        sortedData = new SortedList<>(filteredData);
        sortedData.comparatorProperty().bind(collectionTable.comparatorProperty());
        collectionTable.setItems(sortedData);

        // Обработчики событий
        filterField.textProperty().addListener((observable, oldValue, newValue) -> applyFilter());
        applyFilterButton.setOnAction(event -> applyFilter());
        resetFilterButton.setOnAction(event -> resetFilter());

        logoutButton.setOnAction(event -> clientApp.logout());
        addButton.setOnAction(event -> showAddDialog());
        editButton.setOnAction(event -> showEditDialog());
        removeButton.setOnAction(event -> removeSelectedObject());
        executeButton.setOnAction(event -> executeCommand());

        // Настройка обработчика выбора в таблице
        collectionTable.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> drawVisualization());

        // Настройка обработчика кликов на canvas
        visualizationCanvas.setOnMouseClicked(event -> handleCanvasClick(event.getX(), event.getY()));
    }

    // ... остальные методы остаются без изменений ...

    private void updateTexts() {
        ResourceBundle bundle = LocaleManager.getInstance().getResourceBundle();

        // Обновление текстов элементов
        currentUserLabel.setText(bundle.getString("main.user") + " " + currentUser.login());
        logoutButton.setText(bundle.getString("main.logout"));
        filterField.setPromptText(bundle.getString("main.filter"));
        applyFilterButton.setText(bundle.getString("main.apply_filter"));
        resetFilterButton.setText(bundle.getString("main.reset_filter"));
        addButton.setText(bundle.getString("main.add"));
        editButton.setText(bundle.getString("main.edit"));
        removeButton.setText(bundle.getString("main.remove"));
        commandField.setPromptText(bundle.getString("main.command"));
        executeButton.setText(bundle.getString("main.execute"));
        statusLabel.setText(bundle.getString("main.status.ready"));

        // Обновление заголовков столбцов таблицы
        idColumn.setText(bundle.getString("column.id"));
        nameColumn.setText(bundle.getString("column.name"));
        ownerColumn.setText(bundle.getString("column.creator"));
        xColumn.setText(bundle.getString("column.x"));
        yColumn.setText(bundle.getString("column.y"));
        creationDateColumn.setText(bundle.getString("column.creation_date"));
    }
}