package lab8.client.controllers;

import java.net.URL;
import java.util.Deque;
import java.util.LinkedList;
import java.util.ResourceBundle;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.Alert.AlertType;
import lab8.shared.io.console.ClientConsole;
import lab8.shared.io.console.StdConsole;

public class TerminalController extends ToolbarController {

    @FXML
    private TextField commandInput;

    @FXML
    private TextArea outputArea;

    @FXML
    private Button sendButton;

    private String input = "";
    private Deque<String> outputDeque = new LinkedList<String>();

    public void initialize(URL location, ResourceBundle resources) {
        // super.initialize(location, resources);
        // updateOutputText(output);
        // outputArea.setText("Welcome to terminal of SpaceMarine manager!");
        while (!outputDeque.isEmpty()) {
            writeln(outputDeque.poll());
        }
    }

    @FXML
    private void handleSendCommand(ActionEvent event) {
        input = commandInput.getText();
        if (!input.isBlank()) {
            outputArea.appendText("=> " + input + "\n");
            commandInput.clear();
            ClientConsole.getInstance().add(input);
        }
    }

    public String readInput() {
        String input_read = input;
        input = "";
        return input_read;
    }

    public void write(String output) {
        try {
            outputArea.appendText(output);
        } catch (Exception e) {
            outputDeque.push(output);
            Alert msg = new Alert(AlertType.INFORMATION);
            msg.setContentText(output);
            msg.setTitle("Info");
            msg.show();
            StdConsole.write(output);
            e.printStackTrace();
        }
    }

    public void writeln(String output) {
        write(output + "\n");
    }
}
