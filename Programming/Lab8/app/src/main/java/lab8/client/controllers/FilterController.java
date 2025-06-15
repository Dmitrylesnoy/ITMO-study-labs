package lab8.client.controllers;

import lab8.shared.model.*;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.Stage;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.Predicate;

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
    private Label conditionLabel, valueLabel;

    private TableController tableController;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    public void setTable(TableController tableController) {
        this.tableController = tableController;
        initializeColumnCombo();
    }

    private void initializeColumnCombo() {
        columnCombo.setItems(FXCollections.observableArrayList(
                "id", "name", "coordinates.x", "coordinates.y", "creationDate", "health",
                "loyal", "achievements", "meleeWeapon", "chapter.name", "chapter.world", "creatorId"));
        columnCombo.getSelectionModel().selectedItemProperty()
                .addListener((obs, oldVal, newVal) -> updateConditionCombo(newVal));
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
                conditionCombo.setItems(FXCollections.observableArrayList("Equals", "Greater than", "Less than"));
                valueField.setPromptText("Enter number");
                break;
            case "name":
            case "achievements":
            case "chapter.name":
            case "chapter.world":
                conditionCombo.setItems(FXCollections.observableArrayList("Contains", "Equals"));
                valueField.setPromptText("Enter text");
                break;
            case "loyal":
                conditionCombo.setItems(FXCollections.observableArrayList("Equals"));
                valueField.setVisible(false);
                valueCombo.setVisible(true);
                valueCombo.setItems(FXCollections.observableArrayList("true", "false", "null"));
                valueCombo.setPromptText("Select true/false/null");
                break;
            case "meleeWeapon":
                conditionCombo.setItems(FXCollections.observableArrayList("Equals"));
                valueField.setVisible(false);
                valueCombo.setVisible(true);
                valueCombo.setItems(FXCollections.observableArrayList(
                        "CHAIN_SWORD", "POWER_SWORD", "POWER_BLADE", "POWER_FIST", "null"));
                valueCombo.setPromptText("Select weapon");
                break;
            case "creationDate":
                conditionCombo.setItems(FXCollections.observableArrayList("After", "Before", "Equals"));
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
            showAlert(Alert.AlertType.WARNING, "Invalid Filter", "Please select column, condition, and value.");
            return;
        }

        try {
            Predicate<SpaceMarine> filter = createFilterPredicate(column, condition, value);
            tableController.setFilter(filter);
            closeWindow();
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, "Filter Error", "Failed to apply filter: " + e.getMessage());
        }
    }

    private Predicate<SpaceMarine> createFilterPredicate(String column, String condition, String value)
            throws ParseException {
        switch (column) {
            case "id":
                long idValue = Long.parseLong(value);
                return marine -> {
                    if (marine.getId() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getId() == idValue;
                        case "Greater than" -> marine.getId() > idValue;
                        case "Less than" -> marine.getId() < idValue;
                        default -> false;
                    };
                };
            case "name":
                return marine -> {
                    if (marine.getName() == null)
                        return false;
                    return switch (condition) {
                        case "Contains" -> marine.getName().toLowerCase().contains(value.toLowerCase());
                        case "Equals" -> marine.getName().equalsIgnoreCase(value);
                        default -> false;
                    };
                };
            case "coordinates.x":
                double xValue = Double.parseDouble(value);
                return marine -> {
                    if (marine.getCoordinates() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getCoordinates().getX() == xValue;
                        case "Greater than" -> marine.getCoordinates().getX() > xValue;
                        case "Less than" -> marine.getCoordinates().getX() < xValue;
                        default -> false;
                    };
                };
            case "coordinates.y":
                float yValue = Float.parseFloat(value);
                return marine -> {
                    if (marine.getCoordinates() == null || marine.getCoordinates().getY() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getCoordinates().getY() == yValue;
                        case "Greater than" -> marine.getCoordinates().getY() > yValue;
                        case "Less than" -> marine.getCoordinates().getY() < yValue;
                        default -> false;
                    };
                };
            case "creationDate":
                Date dateValue = dateFormat.parse(value);
                return marine -> {
                    if (marine.getCreationDate() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getCreationDate().equals(dateValue);
                        case "After" -> marine.getCreationDate().after(dateValue);
                        case "Before" -> marine.getCreationDate().before(dateValue);
                        default -> false;
                    };
                };
            case "health":
                double healthValue = Double.parseDouble(value);
                return marine -> {
                    if (marine.getHealth() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getHealth() == healthValue;
                        case "Greater than" -> marine.getHealth() > healthValue;
                        case "Less than" -> marine.getHealth() < healthValue;
                        default -> false;
                    };
                };
            case "loyal":
                Boolean loyalValue = value.equals("null") ? null : Boolean.parseBoolean(value);
                return marine -> condition.equals("Equals") &&
                        (marine.getLoyal() == null ? loyalValue == null : marine.getLoyal().equals(loyalValue));
            case "achievements":
                return marine -> {
                    if (marine.getAchievements() == null)
                        return false;
                    return switch (condition) {
                        case "Contains" -> marine.getAchievements().toLowerCase().contains(value.toLowerCase());
                        case "Equals" -> marine.getAchievements().equalsIgnoreCase(value);
                        default -> false;
                    };
                };
            case "meleeWeapon":
                MeleeWeapon weaponValue = value.equals("null") ? null : MeleeWeapon.valueOf(value);
                return marine -> condition.equals("Equals") &&
                        (marine.getMeleeWeapon() == null ? weaponValue == null
                                : marine.getMeleeWeapon().equals(weaponValue));
            case "chapter.name":
                return marine -> {
                    if (marine.getChapter() == null || marine.getChapter().getName() == null)
                        return false;
                    return switch (condition) {
                        case "Contains" -> marine.getChapter().getName().toLowerCase().contains(value.toLowerCase());
                        case "Equals" -> marine.getChapter().getName().equalsIgnoreCase(value);
                        default -> false;
                    };
                };
            case "chapter.world":
                return marine -> {
                    if (marine.getChapter() == null || marine.getChapter().getWorld() == null)
                        return false;
                    return switch (condition) {
                        case "Contains" -> marine.getChapter().getWorld().toLowerCase().contains(value.toLowerCase());
                        case "Equals" -> marine.getChapter().getWorld().equalsIgnoreCase(value);
                        default -> false;
                    };
                };
            case "creatorId":
                int creatorIdValue = Integer.parseInt(value);
                return marine -> {
                    if (marine.getCreatorId() == null)
                        return false;
                    return switch (condition) {
                        case "Equals" -> marine.getCreatorId() == creatorIdValue;
                        case "Greater than" -> marine.getCreatorId() > creatorIdValue;
                        case "Less than" -> marine.getCreatorId() < creatorIdValue;
                        default -> false;
                    };
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