package lab8.client.controllers;

import lab8.client.utils.Handler;
import lab8.shared.model.*;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import java.text.SimpleDateFormat;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.stream.Collectors;

import javafx.stage.Stage;

public class EditController {

    @FXML
    private TextField idField, nameField, coordXField, coordYField, creationDateField, healthField, achievementsField,
            chapterNameField, chapterWorldField;
    @FXML
    private CheckBox loyalCheckBox;
    @FXML
    private ComboBox<MeleeWeapon> meleeWeaponCombo;

    private SpaceMarine marine;
    private TableController tableController;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize() {
        meleeWeaponCombo.setItems(FXCollections.observableArrayList(MeleeWeapon.values()));
    }

    public void setMarine(SpaceMarine marine, TableController tableController) {
        this.marine = marine;
        this.tableController = tableController;
        populateFields();
    }

    private void populateFields() {
        idField.setText(marine.getId() != null ? marine.getId().toString() : "");
        nameField.setText(marine.getName());
        coordXField.setText(String.valueOf(marine.getCoordinates().getX()));
        coordYField.setText(marine.getCoordinates().getY() != null ? marine.getCoordinates().getY().toString() : "");
        creationDateField.setText(marine.getCreationDate() != null ? dateFormat.format(marine.getCreationDate()) : "");
        healthField.setText(marine.getHealth() != null ? marine.getHealth().toString() : "");
        loyalCheckBox.setSelected(marine.getLoyal() != null ? marine.getLoyal() : false);
        achievementsField.setText(marine.getAchievements());
        meleeWeaponCombo.setValue(marine.getMeleeWeapon());
        if (marine.getChapter() != null) {
            chapterNameField.setText(marine.getChapter().getName());
            chapterWorldField.setText(marine.getChapter().getWorld());
        }
    }

    @FXML
    private void saveMarine() {
        try {
            // Валидация данных
            if (nameField.getText().isEmpty()) {
                showAlert(Alert.AlertType.ERROR, "Validation Error", "Name cannot be empty.");
                return;
            }
            double coordX = Double.parseDouble(coordXField.getText());
            if (coordYField.getText().isEmpty()) {
                showAlert(Alert.AlertType.ERROR, "Validation Error", "Coordinates Y cannot be empty.");
                return;
            }
            Float coordY = Float.parseFloat(coordYField.getText());
            Double health = healthField.getText().isEmpty() ? null : Double.parseDouble(healthField.getText());
            if (health != null && health <= 0) {
                showAlert(Alert.AlertType.ERROR, "Validation Error", "Health must be greater than 0.");
                return;
            }
            if (achievementsField.getText().isEmpty()) {
                showAlert(Alert.AlertType.ERROR, "Validation Error", "Achievements cannot be empty.");
                return;
            }
            String chapterName = chapterNameField.getText();
            if (chapterName.isEmpty()) {
                showAlert(Alert.AlertType.ERROR, "Validation Error", "Chapter name cannot be empty.");
                return;
            }

            // Обновление объекта
            marine.setName(nameField.getText());
            marine.setCoordinates(new Coordinates(coordX, coordY));
            marine.setHealth(health);
            marine.setLoyal(loyalCheckBox.isSelected() ? loyalCheckBox.isSelected() : null);
            marine.setAchievements(achievementsField.getText());
            marine.setMeleeWeapon(meleeWeaponCombo.getValue());
            marine.setChapter(new Chapter(chapterName, chapterWorldField.getText()));

            // Отправка на сервер
            Deque newDeque = new ArrayDeque<SpaceMarine>();
            newDeque = Handler.getInstance().getCollection().stream()
                    .filter(mar -> mar.getId() != marine.getId()).collect(Collectors.toCollection(ArrayDeque::new));
            newDeque.add(marine);
            Handler.getInstance().setCollection(newDeque);            tableController.refreshTable();
            closeWindow();
        } catch (NumberFormatException e) {
            showAlert(Alert.AlertType.ERROR, "Validation Error", "Invalid number format in coordinates or health.");
        } catch (Exception e) {
            showAlert(Alert.AlertType.ERROR, "Error", "Failed to save marine: " + e.getMessage());
        }
    }

    @FXML
    private void cancel() {
        closeWindow();
    }

    private void closeWindow() {
        Stage stage = (Stage) idField.getScene().getWindow();
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