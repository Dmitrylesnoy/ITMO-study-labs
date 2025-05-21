package lab8.client.controllers;

import java.io.IOException;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import lab8.shared.io.console.StdConsole;

public class TerminalController extends ToolbarController {

    @FXML
    private TextField commandInput;

    @FXML
    private TextArea outputArea;

    @FXML
    private Button sendButton;

    private String input = "";

    @FXML
    private void handleSendCommand(ActionEvent event) {
        input = commandInput.getText();
        if (!input.isEmpty()) {
            outputArea.appendText("=> " + input + "\n");
            commandInput.clear();
        }
    }

    @FXML
    public String readInput() throws IOException {
        String input_read = input;
        input = "";
        return input_read;
    }

    @FXML
    public void write(String output) {
        try {
            outputArea.appendText(output);
        } catch (NullPointerException e) {
            StdConsole.write(output);
        }
    }

    @FXML
    public void writeln(String output) {
        write(output);
        write("\n");
    }
}
