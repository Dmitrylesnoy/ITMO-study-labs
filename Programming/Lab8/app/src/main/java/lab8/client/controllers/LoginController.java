package lab8.client.controllers;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import lab8.client.controllers.util.ToolbarController;
import lab8.client.utils.Handler;
import lab8.client.controllers.util.LocalizationManager;

public class LoginController implements Initializable {

    @FXML
    private TextField loginField;
    @FXML
    private PasswordField passwordField;
    @FXML
    private Button loginBtn;
    @FXML
    private Label statusLabel;
    @FXML
    protected ComboBox<String> languageSelector;

    private String username;
    private String password;
    private boolean loginStatus = false;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        // Check for null FXML injections
        if (loginField == null || passwordField == null || loginBtn == null || statusLabel == null
                || languageSelector == null) {
            System.err.println("FXML injection failed in LoginController: " +
                    "loginField=" + loginField + ", passwordField=" + passwordField +
                    ", loginBtn=" + loginBtn + ", statusLabel=" + statusLabel +
                    ", languageSelector=" + languageSelector);
            showAlert(Alert.AlertType.ERROR, "Initialization Error",
                    "Failed to initialize login page. Check FXML configuration.");
            return;
        }

        // Bind localized texts
        loginField.promptTextProperty().bind(LocalizationManager.createStringBinding("login.username"));
        passwordField.promptTextProperty().bind(LocalizationManager.createStringBinding("login.password"));
        loginBtn.textProperty().bind(LocalizationManager.createStringBinding("login.button"));

        // Initialize language selector
        languageSelector.setItems(FXCollections.observableArrayList(
                "Русский", "Nederlands", "Ελληνικά", "Español (Puerto Rico)"));
        languageSelector.setOnAction(this::handleLanguageChange);
    }

    @FXML
    private void handleLanguageChange(ActionEvent event) {
        String selected = languageSelector.getSelectionModel().getSelectedItem();
        if (selected == null)
            return;
        switch (selected) {
            case "Русский":
                LocalizationManager.switchToRussian();
                break;
            case "Nederlands":
                LocalizationManager.switchToDutch();
                break;
            case "Ελληνικά":
                LocalizationManager.switchToGreek();
                break;
            case "Español (Puerto Rico)":
                LocalizationManager.switchToSpanishPR();
                break;
        }
    }

    @FXML
    private void loginClick(ActionEvent event) {
        username = loginField.getText();
        password = passwordField.getText();
        if (username == null || password == null || username.isBlank() || password.isBlank()) {
            setStatusText(LocalizationManager.getString("login.status.empty"));
            return;
        }
        setStatusText(LocalizationManager.getString("login.status.trying"));

        System.out.println("Login: " + username);
        System.out.println("Password: " + password);
        if (Handler.tryLogin(username, password)) {
            setStatusText(LocalizationManager.getString("login.status.success"));
            Stage stage = (Stage) loginBtn.getScene().getWindow();
            stage.close();
            ToolbarController.getInstance().setUser(username);
            ToolbarController.getInstance().openTable(event);
            loginStatus = true;
        } else {
            setStatusText(LocalizationManager.getString("login.status.failed"));
        }
    }

    public static Stage open() {
        Stage stage = ToolbarController.openWindow("/fxml/login.fxml",
                LocalizationManager.getString("login.title"), 900, 700);
        return stage;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public boolean getLoginStatus() {
        return loginStatus;
    }

    public void setStatusText(String text) {
        Platform.runLater(() -> {
            if (statusLabel != null) {
                statusLabel.setText(text);
            }
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