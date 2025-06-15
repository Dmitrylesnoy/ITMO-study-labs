package lab8.client.controllers;

import lab8.client.utils.Handler;
import lab8.shared.commands.Add;
import lab8.shared.messages.Request;
import lab8.shared.model.*;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import java.text.SimpleDateFormat;
import javafx.stage.Stage;

public class EditController {

    @FXML
    private TextField idField, nameField, coordXField, coordYField, creationDateField, healthField, achievementsField,
            chapterNameField, chapterWorldField;
    @FXML private CheckBox loyalCheckBox;
    @FXML private ComboBox<MeleeWeapon> meleeWeaponCombo;
    @FXML Label viewName;

    private static SpaceMarine marine;
    private String mode = "edit";
    private TableController tableController;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize() {
        meleeWeaponCombo.setItems(FXCollections.observableArrayList(MeleeWeapon.values()));
    }

    public EditController setMarine(SpaceMarine marine) {
        this.marine = marine;
        populateFields();
        return this;
    }

    public EditController setTable(TableController tableController) {
        this.tableController = tableController;
        return this;
    }

    public EditController setMode(String mode) {
        switch (mode.toLowerCase()) {
            case "add":
                viewName.setText("Add SpaceMarine");
                mode = "add";
                break;
            default:
                viewName.setText("Edit SpaceMarine");
                mode = "edit";
                break;
        }
        return this;
    }

    private void populateFields() {
        nameField.setText(marine.getName() != null ? marine.getName() : "");
        coordXField.setText(String.valueOf(marine.getCoordinates() != null ? marine.getCoordinates().getX() : ""));
        coordYField.setText(marine.getCoordinates() != null && marine.getCoordinates().getY() != null
                ? marine.getCoordinates().getY().toString()
                : "");
        creationDateField.setText(marine.getCreationDate() != null ? dateFormat.format(marine.getCreationDate()) : "");
        healthField.setText(marine.getHealth() != null ? marine.getHealth().toString() : "");
        loyalCheckBox.setSelected(marine.getLoyal() != null ? marine.getLoyal() : false);
        achievementsField.setText(marine.getAchievements() != null ? marine.getAchievements() : "");
        meleeWeaponCombo.setValue(marine.getMeleeWeapon() != null ? marine.getMeleeWeapon() : null);
        if (marine.getChapter() != null) {
            chapterNameField.setText(marine.getChapter().getName() != null ? marine.getChapter().getName() : "");
            chapterWorldField.setText(marine.getChapter().getWorld() != null ? marine.getChapter().getWorld() : "");
        } else {
            chapterNameField.setText("");
            chapterWorldField.setText("");
        }
    }

    @FXML
    private void saveMarine() {
        try {
            SpaceMarine marine;
            switch (mode) {
                case "edit":
                    marine = editItem();
                    if (marine!=null)
                    Handler.getInstance().updateItem(marine);
                    break;
                case "add":
                    marine = addItem();
                    if (marine!=null)
                        Handler.getInstance().run(new Request(new Add(), marine,null,null));
                default:
                    break;
            }
            tableController.refreshTable();
            closeWindow();
        } catch (NumberFormatException e) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error",
                    "Invalid number format in coordinates or health.");
        } catch (Exception e) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Error", "Failed to save marine: " + e.getMessage());
        }
    }
    
    private SpaceMarine editItem() {
        String name = nameField.getText();
        if (name == null || name.trim().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error", "Name cannot be empty.");
            return null;
        }
        double coordX = Double.parseDouble(coordXField.getText());
        if (coordYField.getText().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error",
                    "Coordinates Y cannot be empty.");
            return null;
        }
        Float coordY = Float.parseFloat(coordYField.getText());
        Double health = healthField.getText().isEmpty() ? null : Double.parseDouble(healthField.getText());
        if (health != null && health <= 0) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error",
                    "Health must be greater than 0.");
            return null;
        }
        if (achievementsField.getText().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error", "Achievements cannot be empty.");
            return null;
        }
        String chapterName = chapterNameField.getText();
        if (chapterName.isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error", "Chapter name cannot be empty.");
            return null;
        }

        marine = new SpaceMarine();
        marine.setName(nameField.getText());
        marine.setCoordinates(new Coordinates(coordX, coordY));
        marine.setHealth(health);
        marine.setLoyal(loyalCheckBox.isSelected() ? loyalCheckBox.isSelected() : null);
        marine.setAchievements(achievementsField.getText());
        marine.setMeleeWeapon(meleeWeaponCombo.getValue());
        marine.setChapter(new Chapter(chapterName, chapterWorldField.getText()));

        return marine;
    }

    private SpaceMarine addItem() {
        SpaceMarine addMarine;
        while (true) {
            addMarine = editItem();
            if (addMarine != null)
                return addMarine;
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
}