package lab8.client.application;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;

public class LoginController {

    @FXML
    private Button btnLogin;


    @FXML
    private void login(ActionEvent event) {
        btnLogin.setText("Loginning...");
    }
}