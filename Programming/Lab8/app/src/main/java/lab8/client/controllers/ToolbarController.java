package lab8.client.controllers;

import java.io.IOException;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

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
    private void openTerminal(ActionEvent event) {
        openWindow(event,"/fxml/terminal.fxml", "Терминал", 900, 700);
    }

    @FXML
    private void openTable(ActionEvent event) {
        openWindow(event,"/fxml/table.fxml", "Таблица", 1200, 700);

    }

    @FXML
    private void openCards(ActionEvent event) {
        openWindow(event,"/fxml/cards.fxml", "Карточки", 900, 700);
    }

    private void openWindow(ActionEvent event,String fxmlPath, String title, int w, int h) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource(fxmlPath));
            Parent root = loader.load();

            Stage stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
            stage.setScene(new Scene(root,w,h));
            stage.setTitle(title);
            stage.show();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
