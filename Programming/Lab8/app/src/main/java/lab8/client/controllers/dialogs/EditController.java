package lab8.client.controllers.dialogs;

import java.text.SimpleDateFormat;

import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import lab8.client.controllers.TableController;
import lab8.client.controllers.util.ToolbarController;
import lab8.client.utils.Handler;
import lab8.shared.commands.Add;
import lab8.shared.io.console.StdConsole;
import lab8.shared.messages.Request;
import lab8.shared.model.Chapter;
import lab8.shared.model.Coordinates;
import lab8.shared.model.MeleeWeapon;
import lab8.shared.model.SpaceMarine;

public class EditController {

    @FXML
    private TextField idField, nameField, coordXField, coordYField, creationDateField, healthField, achievementsField,
            chapterNameField, chapterWorldField;
    @FXML
    private CheckBox loyalCheckBox;
    @FXML
    private ComboBox<MeleeWeapon> meleeWeaponCombo;
    @FXML
    Label viewName;

    private static SpaceMarine marine;
    private boolean modeNew = false;
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

    public EditController setMode(boolean mode) {
        this.modeNew = mode;
        if (modeNew)
            viewName.setText("Add SpaceMarine");
        else
            viewName.setText("Edit SpaceMarine");

        StdConsole.writeln(modeNew ? "true" : "fasle");
        return this;
    }

    private void populateFields() {
        idField.setText(marine.getId() != null ? marine.getId().toString() : "");
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
            SpaceMarine marine=editItem(modeNew);
            if (marine != null) {
                if (!modeNew) {
                    Handler.getInstance().updateItem(marine);
                } else {
                    Handler.getInstance().run(new Request(new Add(), marine, null, null));
                }
                closeWindow();
            }
            tableController.refreshTable();
        } catch (NumberFormatException e) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Validation Error",
                    "Invalid number format in coordinates.");
        } catch (Exception e) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, "Error", "Failed to save marine: " + e.getMessage());
        }
    }

    private SpaceMarine editItem(boolean mode) {
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
        Float coordY = Float.parseFloat(coordYField.getText()); //  TODO ограничения htalth
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

        if (!mode) {
            marine.setName(nameField.getText());
            marine.setCoordinates(new Coordinates(coordX, coordY));
            marine.setHealth(health);
            marine.setLoyal(loyalCheckBox.isSelected() ? loyalCheckBox.isSelected() : null);
            marine.setAchievements(achievementsField.getText());
            marine.setMeleeWeapon(meleeWeaponCombo.getValue());
            marine.setChapter(new Chapter(chapterName, chapterWorldField.getText()));
        } else {
            marine = new SpaceMarine(nameField.getText(), new Coordinates(coordX, coordY), health,
                    loyalCheckBox.isSelected(),
                    achievementsField.getText(), meleeWeaponCombo.getValue(),
                    new Chapter(chapterName, chapterWorldField.getText()));

        }
        return marine;
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