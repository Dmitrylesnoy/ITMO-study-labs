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
import lombok.Getter;

@Getter
public class ToolbarController {

    @FXML private HBox toolbar;
    @FXML private Label userLabel;
    @FXML private Button terminalBtn;
    @FXML private Button tableBtn;
    @FXML private Button cardsBtn;

    private static ToolbarController instance;
    private static Stage terminalStage=openWindow("/fxml/terminal.fxml", "Table", 900, 700);
    private static Stage tableStage = openWindow("/fxml/table.fxml", "Table", 1200, 700);
    private static Stage cardsStage = openWindow("/fxml/cards.fxml", "Cards", 900, 700);
  
    @FXML
    public void openTerminal(ActionEvent event) {
        closeWindow(event);
        terminalStage.show();
    }

    @FXML
    public void openTable(ActionEvent event) {
        closeWindow(event);
        tableStage.show();
    }

    @FXML
    public void openCards(ActionEvent event) {
        closeWindow(event);
        cardsStage.show();
    }

    public void closeWindow(ActionEvent event) {
        ((Stage) ((Node) event.getSource()).getScene().getWindow()).close();
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
