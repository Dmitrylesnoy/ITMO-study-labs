package lab8.client.controllers;

import java.io.IOException;

import javax.tools.Tool;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import lombok.Getter;

@Getter
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

    private static ToolbarController instance = new ToolbarController();
    private Stage terminal_stage = openWindow("/fxml/terminal.fxml", "Table", 1200, 700);
    private Stage table_stage = openWindow("/fxml/table.fxml", "Table", 1200, 700);
    private Stage cards_stage = openWindow("/fxml/cards.fxml", "Cards", 900, 700);
    
    @FXML
    public void openTerminal(ActionEvent event) {
        terminal_stage.show();
    }

    @FXML
    public void openTable(ActionEvent event) {
        table_stage.show();
    }

    @FXML
    public void openCards(ActionEvent event) {
        cards_stage.show();
    }

    @FXML
    public void setUser(String username) {
        userLabel.setText(username);
    }

    public static Stage openWindow(String fxmlPath, String title, int w, int h) {
        try {
            FXMLLoader loader = new FXMLLoader(ToolbarController.class.getResource(fxmlPath));
            Parent root = loader.load();

            // stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
            Stage stage = new Stage();
            stage.setScene(new Scene(root, w, h));
            stage.setTitle(title);
            // stage.show();
            return stage;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static ToolbarController getInstance() {
        return instance == null ? instance = new ToolbarController() : instance;
    }
}
