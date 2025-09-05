package lab8.client.controllers.dialogs;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import lab8.client.controllers.TableController;
import lab8.client.controllers.util.LocalizationManager;
import lab8.shared.model.MeleeWeapon;
import lab8.shared.model.SpaceMarine;

public class FilterController {

    @FXML
    private ComboBox<String> columnCombo;
    @FXML
    private ComboBox<String> conditionCombo;
    @FXML
    private TextField valueField;
    @FXML
    private ComboBox<String> valueCombo;
    @FXML
    private DatePicker datePicker;
    @FXML
    private Label conditionLabel;
    @FXML
    private Label valueLabel;
    private TableController tableController;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    private final Map<String, String> conditionKeyMap = new HashMap<>();

    public void setTable(TableController tableController) {
        this.tableController = tableController;
        initializeColumnCombo();
        initializeConditionKeyMap();
    }

    private void initializeColumnCombo() {
        columnCombo.setItems(FXCollections.observableArrayList(
                "id", "name", "coordinates.x", "coordinates.y", "creationDate", "health",
                "loyal", "achievements", "meleeWeapon", "chapter.name", "chapter.world", "creatorId"));
        columnCombo.getSelectionModel().selectedItemProperty()
                .addListener((obs, oldVal, newVal) -> updateConditionCombo(newVal));

        // Bind localized texts
        columnCombo.promptTextProperty().bind(LocalizationManager.createStringBinding("filter.value.select"));
        conditionLabel.textProperty().bind(LocalizationManager.createStringBinding("filter.condition.equals"));
        valueLabel.textProperty().bind(LocalizationManager.createStringBinding("filter.value.text"));
    }

    private void initializeConditionKeyMap() {
        // Map localized condition strings to their resource bundle keys
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.equals"), "equals");
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.greater"), "greater");
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.less"), "less");
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.contains"), "contains");
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.after"), "after");
        conditionKeyMap.put(LocalizationManager.getString("filter.condition.before"), "before");
    }

    private void updateConditionCombo(String column) {
        conditionCombo.getItems().clear();
        valueField.setVisible(true);
        valueCombo.setVisible(false);
        datePicker.setVisible(false);

        if (column == null)
            return;

        switch (column) {
            case "id":
            case "coordinates.x":
            case "coordinates.y":
            case "health":
            case "creatorId":
                conditionCombo.setItems(FXCollections.observableArrayList(
                        LocalizationManager.getString("filter.condition.equals"),
                        LocalizationManager.getString("filter.condition.greater"),
                        LocalizationManager.getString("filter.condition.less")));
                valueField.setPromptText(LocalizationManager.getString("filter.value.number"));
                break;
            case "name":
            case "achievements":
            case "chapter.name":
            case "chapter.world":
                conditionCombo.setItems(FXCollections.observableArrayList(
                        LocalizationManager.getString("filter.condition.contains"),
                        LocalizationManager.getString("filter.condition.equals")));
                valueField.setPromptText(LocalizationManager.getString("filter.value.text"));
                break;
            case "loyal":
                conditionCombo.setItems(FXCollections.observableArrayList(
                        LocalizationManager.getString("filter.condition.equals")));
                valueField.setVisible(false);
                valueCombo.setVisible(true);
                valueCombo.setItems(FXCollections.observableArrayList("true", "false", "null"));
                valueCombo.setPromptText(LocalizationManager.getString("filter.value.select"));
                break;
            case "meleeWeapon":
                conditionCombo.setItems(FXCollections.observableArrayList(
                        LocalizationManager.getString("filter.condition.equals")));
                valueField.setVisible(false);
                valueCombo.setVisible(true);
                valueCombo.setItems(FXCollections.observableArrayList(
                        "CHAIN_SWORD", "POWER_SWORD", "POWER_BLADE", "POWER_FIST", "null"));
                valueCombo.setPromptText(LocalizationManager.getString("filter.value.weapon"));
                break;
            case "creationDate":
                conditionCombo.setItems(FXCollections.observableArrayList(
                        LocalizationManager.getString("filter.condition.after"),
                        LocalizationManager.getString("filter.condition.before"),
                        LocalizationManager.getString("filter.condition.equals")));
                valueField.setVisible(false);
                datePicker.setVisible(true);
                break;
        }
    }

    @FXML
    private void applyFilter() {
        String column = columnCombo.getValue();
        String condition = conditionCombo.getValue();
        String value = valueField.isVisible() ? valueField.getText() : valueCombo.getValue();
        if (datePicker.isVisible()) {
            value = datePicker.getValue() != null ? datePicker.getValue().toString() : "";
        }

        if (column == null || condition == null || value == null || value.isEmpty()) {
            showAlert(Alert.AlertType.WARNING, LocalizationManager.getString("filter.warning.invalid"),
                    LocalizationManager.getString("filter.warning.invalid"));
            return;
        }

        try {
            Predicate<SpaceMarine> filter = createFilterPredicate(column, condition, value);
            tableController.setFilter(filter);
            closeWindow();
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, LocalizationManager.getString("filter.error"),
                    LocalizationManager.getString("filter.error") + ": " + e.getMessage());
        }
    }

private Predicate<SpaceMarine> createFilterPredicate(String column, String condition, String value)
        throws ParseException {
    String conditionKey = conditionKeyMap.getOrDefault(condition, "");
    
    switch (column) {
        case "id":
            long idValue = Long.parseLong(value);
            return marine -> {
                if (marine.getId() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getId() == idValue;
                    case "greater":
                        return marine.getId() > idValue;
                    case "less":
                        return marine.getId() < idValue;
                    default:
                        return false;
                }
            };
        case "name":
            return marine -> {
                if (marine.getName() == null)
                    return false;
                switch (conditionKey) {
                    case "contains":
                        return marine.getName().toLowerCase().contains(value.toLowerCase());
                    case "equals":
                        return marine.getName().equalsIgnoreCase(value);
                    default:
                        return false;
                }
            };
        case "coordinates.x":
            double xValue = Double.parseDouble(value);
            return marine -> {
                if (marine.getCoordinates() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getCoordinates().getX() == xValue;
                    case "greater":
                        return marine.getCoordinates().getX() > xValue;
                    case "less":
                        return marine.getCoordinates().getX() < xValue;
                    default:
                        return false;
                }
            };
        case "coordinates.y":
            float yValue = Float.parseFloat(value);
            return marine -> {
                if (marine.getCoordinates() == null || marine.getCoordinates().getY() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getCoordinates().getY() == yValue;
                    case "greater":
                        return marine.getCoordinates().getY() > yValue;
                    case "less":
                        return marine.getCoordinates().getY() < yValue;
                    default:
                        return false;
                }
            };
        case "creationDate":
            Date dateValue = dateFormat.parse(value);
            return marine -> {
                if (marine.getCreationDate() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getCreationDate().equals(dateValue);
                    case "after":
                        return marine.getCreationDate().after(dateValue);
                    case "before":
                        return marine.getCreationDate().before(dateValue);
                        default:
                            return false;
                }
            };
        case "health":
            double healthValue = Double.parseDouble(value);
            return marine -> {
                if (marine.getHealth() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getHealth() == healthValue;
                    case "greater":
                        return marine.getHealth() > healthValue;
                    case "less":
                        return marine.getHealth() < healthValue;
                    default:
                        return false;
                }
            };
        case "loyal":
            Boolean loyalValue = value.equals("null") ? null : Boolean.parseBoolean(value);
            return marine -> conditionKey.equals("equals") &&
                    (marine.getLoyal() == null ? loyalValue == null : marine.getLoyal().equals(loyalValue));
        case "achievements":
            return marine -> {
                if (marine.getAchievements() == null)
                    return false;
                switch (conditionKey) {
                    case "contains":
                        return marine.getAchievements().toLowerCase().contains(value.toLowerCase());
                    case "equals":
                        return marine.getAchievements().equalsIgnoreCase(value);
                    default:
                        return false;
                }
            };
        case "meleeWeapon":
            MeleeWeapon weaponValue = value.equals("null") ? null : MeleeWeapon.valueOf(value);
            return marine -> conditionKey.equals("equals") && (marine.getMeleeWeapon() == null ? weaponValue == null : marine.getMeleeWeapon().equals(weaponValue));
        case "chapter.name":
            return marine -> {
                if (marine.getChapter() == null || marine.getChapter().getName() == null)
                    return false;
                switch (conditionKey) {
                    case "contains":
                        return marine.getChapter().getName().toLowerCase().contains(value.toLowerCase());
                    case "equals":
                        return marine.getChapter().getName().equalsIgnoreCase(value);
                    default:
                        return false;
                }
            };
        case "chapter.world":
            return marine -> {
                if (marine.getChapter() == null || marine.getChapter().getWorld() == null)
                    return false;
                switch (conditionKey) {
                    case "contains":
                        return marine.getChapter().getWorld().toLowerCase().contains(value.toLowerCase());
                    case "equals":
                        return marine.getChapter().getWorld().equalsIgnoreCase(value);
                    default:
                        return false;
                }
            };
        case "creatorId":
            int creatorIdValue = Integer.parseInt(value);
            return marine -> {
                if (marine.getCreatorId() == null)
                    return false;
                switch (conditionKey) {
                    case "equals":
                        return marine.getCreatorId() == creatorIdValue;
                    case "greater":
                        return marine.getCreatorId() > creatorIdValue;
                    case "less":
                        return marine.getCreatorId() < creatorIdValue;
                    default:
                        return false;
                }
            };
        default:
            return marine -> true;
    }
    }

    @FXML
    private void resetFilter() {
        tableController.resetFilter();
        closeWindow();
    }

    @FXML
    private void cancel() {
        closeWindow();
    }

    private void closeWindow() {
        Stage stage = (Stage) columnCombo.getScene().getWindow();
        stage.close();
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