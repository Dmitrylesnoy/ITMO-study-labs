package lab8.client.controllers;

import lab8.shared.model.*;
import javafx.beans.property.*;
import javafx.collections.*;
import javafx.fxml.*;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;

import java.net.URL;
import java.util.*;

public class TableController extends ToolbarController implements Initializable {

    @FXML
    private TableView<SpaceMarine> tableView;
    @FXML
    private TableColumn<SpaceMarine, Long> colId;
    @FXML
    private TableColumn<SpaceMarine, String> colName;
    @FXML
    private TableColumn<SpaceMarine, Double> colX;
    @FXML
    private TableColumn<SpaceMarine, Float> colY;
    @FXML
    private TableColumn<SpaceMarine, Date> colCreationDate;
    @FXML
    private TableColumn<SpaceMarine, Double> colHealth;
    @FXML
    private TableColumn<SpaceMarine, Boolean> colLoyal;
    @FXML
    private TableColumn<SpaceMarine, String> colAchievements;
    @FXML
    private TableColumn<SpaceMarine, MeleeWeapon> colWeapon;
    @FXML
    private TableColumn<SpaceMarine, String> colChapterName;
    @FXML
    private TableColumn<SpaceMarine, String> colChapterWorld;
    @FXML
    private TableColumn<SpaceMarine, Integer> colCreatorId;

    private ObservableList<SpaceMarine> marineData = FXCollections.observableArrayList();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        colId.setCellValueFactory(new PropertyValueFactory<>("id"));
        colName.setCellValueFactory(new PropertyValueFactory<>("name"));
        colCreationDate.setCellValueFactory(new PropertyValueFactory<>("creationDate"));
        colHealth.setCellValueFactory(new PropertyValueFactory<>("health"));
        colLoyal.setCellValueFactory(new PropertyValueFactory<>("loyal"));
        colAchievements.setCellValueFactory(new PropertyValueFactory<>("achievements"));
        colWeapon.setCellValueFactory(new PropertyValueFactory<>("meleeWeapon"));
        colCreatorId.setCellValueFactory(new PropertyValueFactory<>("creator_id"));

        colX.setCellValueFactory(cell -> new SimpleDoubleProperty(cell.getValue().getCoordinates().getX()).asObject());
        colY.setCellValueFactory(cell -> new SimpleFloatProperty(cell.getValue().getCoordinates().getY()).asObject());
        colChapterName.setCellValueFactory(cell -> {
            Chapter c = cell.getValue().getChapter();
            return new SimpleStringProperty(c != null ? c.getName() : "");
        });
        colChapterWorld.setCellValueFactory(cell -> {
            Chapter c = cell.getValue().getChapter();
            return new SimpleStringProperty(c != null ? c.getWorld() : "");
        });

        tableView.getColumns().forEach(col -> col.setSortable(true));

        tableView.setItems(marineData);
    }


    public void setSpaceMarines(List<SpaceMarine> list) {
        marineData.setAll(list);
    }

    public void refreshTable() {
        tableView.refresh();
    }
}
