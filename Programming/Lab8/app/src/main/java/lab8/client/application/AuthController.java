package lab8.client.application;

import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

public class AuthController {
    @FXML
    private TabPane authTabPane;
    @FXML
    private Tab loginTab;
    @FXML
    private Tab registerTab;
    @FXML
    private TextField loginField;
    @FXML
    private PasswordField passwordField;
    @FXML
    private Button loginButton;
    @FXML
    private Label authErrorLabel;
    @FXML
    private TextField regLoginField;
    @FXML
    private PasswordField regPasswordField;
    @FXML
    private PasswordField regConfirmPasswordField;
    @FXML
    private Button registerButton;
    @FXML
    private Label regErrorLabel;
    @FXML
    private Label regSuccessLabel;

    private Stage stage;
    private ClientApp clientApp;

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public void setClientApp(ClientApp clientApp) {
        this.clientApp = clientApp;
    }

    @FXML
    private void initialize() {
        updateTexts();
        loginButton.setOnAction(event -> handleLogin());
        registerButton.setOnAction(event -> handleRegister());
    }

    private void handleLogin() {
        String login = loginField.getText();
        String password = passwordField.getText();

        if (login.isEmpty() || password.isEmpty()) {
            showAuthError("Логин и пароль не могут быть пустыми");
            return;
        }

        try {
            boolean success = clientApp.authenticate(login, password);
            if (success) {
                clientApp.showMainWindow();
                stage.close();
            } else {
                showAuthError("Неверный логин или пароль");
            }
        } catch (Exception e) {
            showAuthError("Ошибка соединения с сервером");
            e.printStackTrace();
        }
    }

    private void handleRegister() {
        String login = regLoginField.getText();
        String password = regPasswordField.getText();
        String confirmPassword = regConfirmPasswordField.getText();

        if (login.isEmpty() || password.isEmpty()) {
            showRegError("Логин и пароль не могут быть пустыми");
            return;
        }

        if (!password.equals(confirmPassword)) {
            showRegError("Пароли не совпадают");
            return;
        }

        try {
            boolean success = clientApp.register(login, password);
            if (success) {
                regErrorLabel.setVisible(false);
                regSuccessLabel.setVisible(true);
            } else {
                showRegError("Пользователь с таким логином уже существует");
            }
        } catch (Exception e) {
            showRegError("Ошибка соединения с сервером");
            e.printStackTrace();
        }
    }

    private void showAuthError(String message) {
        authErrorLabel.setText(message);
        authErrorLabel.setVisible(true);
    }

    private void showRegError(String message) {
        regErrorLabel.setText(message);
        regErrorLabel.setVisible(true);
        regSuccessLabel.setVisible(false);
    }

    private void updateTexts() {
        ResourceBundle bundle = LocaleManager.getInstance().getResourceBundle();

        // Обновление текстов элементов
        loginTab.setText(bundle.getString("auth.login"));
        registerTab.setText(bundle.getString("auth.register"));
        loginField.setPromptText(bundle.getString("auth.username"));
        passwordField.setPromptText(bundle.getString("auth.password"));
        loginButton.setText(bundle.getString("auth.sign_in"));
        regLoginField.setPromptText(bundle.getString("auth.username"));
        regPasswordField.setPromptText(bundle.getString("auth.password"));
        regConfirmPasswordField.setPromptText(bundle.getString("auth.confirm_password"));
        registerButton.setText(bundle.getString("auth.register"));
    }
}