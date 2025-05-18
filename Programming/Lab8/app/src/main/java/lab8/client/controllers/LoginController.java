package lab8.client.controllers;

import java.io.IOException;
import java.net.URL;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

public class LoginController {

    @FXML
    private TextField loginField;

    @FXML
    private PasswordField passwordField;

    @FXML
    private Button loginBtn;

    @FXML
    private void loginClick(ActionEvent event) throws IOException {
        String login = loginField.getText();
        String password = passwordField.getText();
        if (login.isBlank() || password.isBlank()) return;
        System.out.println("Login: " + login);
        System.out.println("Password: " + password);

        Parent root = FXMLLoader.load(getClass().getResource("/fxml/terminal.fxml"));
        System.out.println("loaded a terminal stage");
        Stage stage = new Stage();
        stage.setScene(new Scene(root, 900, 700));
        stage.setTitle("Terminal");
        stage.show();
        System.out.println("Showed a new stage");
        ((Stage) loginBtn.getScene().getWindow()).close();
    }
}
