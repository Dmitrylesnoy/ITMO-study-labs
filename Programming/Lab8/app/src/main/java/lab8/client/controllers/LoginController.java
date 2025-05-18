package lab8.client.controllers;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;

public class LoginController {

    @FXML
    private TextField loginField;

    @FXML
    private PasswordField passwordField;

    @FXML
    private Button loginBtn;

    @FXML
    private void loginClick(ActionEvent event) {
        String login = loginField.getText();
        String password = passwordField.getText();
        System.out.println("Login: " + login);
        System.out.println("Password: " + password);
    }
}
