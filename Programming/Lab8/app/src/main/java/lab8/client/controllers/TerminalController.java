package lab8.client.controllers;

import javafx.fxml.FXML;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

public class TerminalController extends ToolbarController {

    @FXML
    private TextField commandInput;

    @FXML
    private TextArea outputArea;

    @FXML
    private void openTerminal() {
        outputArea.appendText("Терминал открыт\n");
    }

    @FXML
    private void openTable() {
        outputArea.appendText("Таблица открыта\n");
    }

    @FXML
    private void openCards() {
        outputArea.appendText("Карточки открыты\n");
    }
}
