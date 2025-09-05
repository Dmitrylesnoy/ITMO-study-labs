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
import lab8.client.controllers.util.LocalizationManager;
import lab8.shared.commands.Add;
import lab8.shared.io.console.StdConsole;
import lab8.shared.messages.Request;
import lab8.shared.model.Chapter;
import lab8.shared.model.Coordinates;
import lab8.shared.model.MeleeWeapon;
import lab8.shared.model.SpaceMarine;

public class EditController {

    @FXML
    private TextField idField;
    @FXML
    private TextField nameField;
    @FXML
    private TextField coordXField;
    @FXML
    private TextField coordYField;
    @FXML
    private TextField creationDateField;
    @FXML
    private TextField healthField;
    @FXML
    private TextField achievementsField;
    @FXML
    private TextField chapterNameField;
    @FXML
    private TextField chapterWorldField;
    @FXML
    private CheckBox loyalCheckBox;
    @FXML
    private ComboBox<MeleeWeapon> meleeWeaponCombo;
    @FXML
    private Label viewName;

    private static SpaceMarine marine;
    private boolean modeNew = false;
    private TableController tableController;
    private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @FXML
    public void initialize() {
        meleeWeaponCombo.setItems(FXCollections.observableArrayList(MeleeWeapon.values()));
        // Bind localized texts
        viewName.textProperty().bind(LocalizationManager.createStringBinding(
                modeNew ? "edit.title.add" : "edit.title.edit"));
        idField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.id"));
        nameField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.name"));
        coordXField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.coordinates.x"));
        coordYField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.coordinates.y"));
        creationDateField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.creationDate"));
        healthField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.health"));
        achievementsField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.achievements"));
        chapterNameField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.chapter.name"));
        chapterWorldField.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.chapter.world"));
        loyalCheckBox.textProperty().bind(LocalizationManager.createStringBinding("edit.loyal"));
        meleeWeaponCombo.promptTextProperty().bind(LocalizationManager.createStringBinding("edit.meleeWeapon"));
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
        viewName.textProperty().bind(LocalizationManager.createStringBinding(
                modeNew ? "edit.title.add" : "edit.title.edit"));
        StdConsole.writeln(modeNew ? "true" : "false");
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
            SpaceMarine marine = editItem(modeNew);
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
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.number_format"),
                    LocalizationManager.getString("edit.error.number_format"));
        } catch (Exception e) {
            ToolbarController.showAlert(Alert.AlertType.ERROR, LocalizationManager.getString("edit.error.save_failed"),
                    LocalizationManager.getString("edit.error.save_failed") + ": " + e.getMessage());
        }
    }

    private SpaceMarine editItem(boolean mode) {
        String name = nameField.getText();
        if (name == null || name.trim().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.validation.name"),
                    LocalizationManager.getString("edit.error.validation.name"));
            return null;
        }
        double coordX = Double.parseDouble(coordXField.getText());
        if (coordYField.getText().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.validation.coordY"),
                    LocalizationManager.getString("edit.error.validation.coordY"));
            return null;
        }
        Float coordY = Float.parseFloat(coordYField.getText());
        Double health = healthField.getText().isEmpty() ? null : Double.parseDouble(healthField.getText());
        if (health != null && health <= 0) {
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.validation.health"),
                    LocalizationManager.getString("edit.error.validation.health"));
            return null;
        }
        if (achievementsField.getText().isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.validation.achievements"),
                    LocalizationManager.getString("edit.error.validation.achievements"));
            return null;
        }
        String chapterName = chapterNameField.getText();
        if (chapterName.isEmpty()) {
            ToolbarController.showAlert(Alert.AlertType.ERROR,
                    LocalizationManager.getString("edit.error.validation.chapter"),
                    LocalizationManager.getString("edit.error.validation.chapter"));
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