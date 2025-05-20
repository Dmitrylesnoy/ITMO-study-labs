package lab8.client.controllers;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public class ToolbarController {

    @FXML
    private HBox toolbar;
    @FXML
    private Label userLabel;
    @FXML
    private Button terminalBtn;
    @FXML
    private Button tableBtn;
    @FXML
    private Button cardsBtn;

    @FXML
    private void openTerminal() {
        openWindow("/fxml/terminal.fxml", "Терминал", 900, 700);
    }

    @FXML
    private void openTable() {
        openWindow("/fxml/table.fxml", "Таблица", 1000, 700);
    }

    @FXML
    private void openCards() {
        openWindow("/fxml/cards.fxml", "Карточки", 900, 700);
    }

    private void openWindow(String fxmlPath, String title, int w, int h) {
        try {
            Parent root = FXMLLoader.load(getClass().getResource(fxmlPath));
            Stage stage = new Stage();
            stage.setScene(new Scene(root, w,h));
            stage.setTitle(title);
            stage.show();
            System.out.println("Showed a new stage");
            ((Stage) toolbar.getScene().getWindow()).close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
