package lab8.client.controllers.util;

import java.io.IOException;
import java.net.URL;
import java.util.HashSet;
import java.util.ResourceBundle;
import java.util.Set;

import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
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
import javafx.scene.control.ComboBox;
import javafx.collections.FXCollections;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import lab8.client.controllers.LoginController;
import lombok.Getter;

@Getter
public class ToolbarController implements Initializable {

    @FXML
    protected HBox toolbar;
    @FXML
    protected Label userLabel;
    @FXML
    protected Button terminalBtn;
    @FXML
    protected Button tableBtn;
    @FXML
    protected Button cardsBtn;
    @FXML
    protected Button themeButton;
    @FXML
    protected ComboBox<String> languageSelector;

    protected static StringProperty usernameProperty = new SimpleStringProperty("");
    protected static ToolbarController instance;
    protected static boolean isLightTheme = true;

    protected static Stage terminalStage = openWindow("/fxml/terminal.fxml",
            LocalizationManager.getString("terminal.title"), 900, 700);
    protected static Stage tableStage = openWindow("/fxml/table.fxml", LocalizationManager.getString("table.title"),
            1000, 700);
    protected static Stage cardsStage = openWindow("/fxml/cards.fxml", LocalizationManager.getString("cards.title"),
            null, null);

    @FXML
    public void initialize(URL location, ResourceBundle resources) {
        instance = this;
        // Bind userLabel to a combination of localized "toolbar.user" and username
        userLabel.textProperty().bind(LocalizationManager.createStringBinding("toolbar.user")
                .concat(": ")
                .concat(usernameProperty));
        terminalBtn.textProperty().bind(LocalizationManager.createStringBinding("toolbar.terminal"));
        tableBtn.textProperty().bind(LocalizationManager.createStringBinding("toolbar.table"));
        cardsBtn.textProperty().bind(LocalizationManager.createStringBinding("toolbar.cards"));
        themeButton.textProperty().bind(LocalizationManager.createStringBinding("toolbar.theme"));
        languageSelector.setItems(FXCollections.observableArrayList(
                "Русский", "Nederlands", "Ελληνικά", "Español (Puerto Rico)"));
        languageSelector.setOnAction(this::handleLanguageChange);
        userLabel.setOnMouseClicked(e -> {
            Alert dialog = new Alert(Alert.AlertType.CONFIRMATION,
                    LocalizationManager.getString("toolbar.logout.confirm"), ButtonType.YES,
                    ButtonType.NO);
            dialog.setTitle(LocalizationManager.getString("toolbar.logout.confirm"));
            dialog.showAndWait().ifPresent(response -> {
                if (response == ButtonType.YES) {
                    ((Stage) userLabel.getScene().getWindow()).close();
                    LoginController.open().show();
                } else {
                    System.out.println(LocalizationManager.getString("toolbar.logout.cancelled"));
                }
            });
        });
    }

    @FXML
    private void handleLanguageChange(ActionEvent event) {
        String selected = languageSelector.getSelectionModel().getSelectedItem();
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
        usernameProperty.set(user);
    }

    public static Stage openWindow(String fxmlPath, String title, Integer w, Integer h) {
        FXMLLoader loader = new FXMLLoader(ToolbarController.class.getResource(fxmlPath));
        return openWindow(loader, title, w, h);
    }

    public static Stage openWindow(FXMLLoader loader, String title, Integer w, Integer h) {
        try {
            Parent root = loader.load();
            Stage stage = new Stage();
            if (w == null && h == null)
                stage.setScene(new Scene(root));
            else
                stage.setScene(new Scene(root, w, h));
            stage.setTitle(title);
            return stage;
        } catch (IOException e) {
            System.err.println("Failed to load FXML: " + loader.getLocation().toExternalForm());
            e.printStackTrace();
            return null;
        }
    }

    public static ToolbarController getInstance() {
        return instance == null ? instance = new ToolbarController() : instance;
    }

    @FXML
    public void toggleTheme(ActionEvent event) {
        isLightTheme = !isLightTheme;
        updateStageStylesheet(((Stage) ((Node) event.getSource()).getScene().getWindow()));
    }

    private static void updateStageStylesheet(Stage stage) {
        if (stage != null && stage.getScene() != null) {
            stage.getScene().getStylesheets().clear();
            String stylesheet = isLightTheme ? "/css/light-theme.css" : "/css/dark-theme.css";
            stage.getScene().getStylesheets().add(ToolbarController.class.getResource(stylesheet).toExternalForm());
        }
    }

    public static void showAlert(Alert.AlertType type, String title, String message) {
        Platform.runLater(() -> {
            Alert alert = new Alert(type);
            alert.setTitle(title);
            alert.setHeaderText(null);
            alert.setContentText(message);
            alert.showAndWait();
        });
    }
}