package lab8.client.controllers;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import lombok.Getter;

@Getter
public class ToolbarController implements Initializable {

    @FXML protected HBox toolbar;
    @FXML protected Label userLabel;
    @FXML protected Button terminalBtn;
    @FXML protected Button tableBtn;
    @FXML protected Button cardsBtn;

    protected static String username = "";
    protected static ToolbarController instance;
    protected static Stage terminalStage=openWindow("/fxml/terminal.fxml", "Table", 900, 700);
    protected static Stage tableStage = openWindow("/fxml/table.fxml", "Table", 1200, 700);
    protected static Stage cardsStage = openWindow("/fxml/cards.fxml", "Cards", 900, 700);
  
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
    public void setUser(String user) {
        ToolbarController.username = user;
        ((Label) terminalStage.getScene().getRoot().lookup("#userLabel")).setText("User: " + username);
        ((Label) tableStage.getScene().getRoot().lookup("#userLabel")).setText("User: " + username);
        ((Label) cardsStage.getScene().getRoot().lookup("#userLabel")).setText("User: " + username);
    }

    public static Stage openWindow(String fxmlPath, String title, int w, int h) {
        try {
            FXMLLoader loader = new FXMLLoader(ToolbarController.class.getResource(fxmlPath));
            Parent root = loader.load();
            Stage stage = new Stage();
            stage.setScene(new Scene(root, w, h));
            stage.setTitle(title);
            return stage;
        } catch (IOException e) {
            System.err.println("Failed to load FXML: " + fxmlPath);
            e.printStackTrace();
            return null;
        }
    }

    public static ToolbarController getInstance() {
        return instance == null ? instance = new ToolbarController() : instance;
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        instance = this;
        userLabel.setOnMouseClicked(e -> {
            Alert dialog = new Alert(Alert.AlertType.CONFIRMATION, "Are you sure you want to log out?", ButtonType.YES, ButtonType.NO);
            dialog.showAndWait().ifPresent(response -> {
                if (response == ButtonType.YES) {
                    LoginController.open().show();
                } else {
                    System.out.println("Logout cancelled.");
                }
            });
        });
    }
}