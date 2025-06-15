package lab8.client.controllers;

import java.net.URL;
import java.util.Deque;
import java.util.LinkedList;
import java.util.ResourceBundle;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import lab8.client.controllers.util.ToolbarController;
import lab8.client.utils.Handler;
import lab8.shared.io.console.ClientConsole;
import lab8.shared.io.console.StdConsole;

public class TerminalController extends ToolbarController {

    @FXML
    private TextField commandInput;

    @FXML
    private TextArea outputArea;

    @FXML
    private Button sendButton;

    private static String input = "";
    private static Deque<String> outputDeque = new LinkedList<String>();
    private static TerminalController instance;

    public void initialize(URL location, ResourceBundle resources) {
        super.initialize(location, resources);
        // updateOutputText(output);
        // outputArea.setText("Welcome to terminal of SpaceMarine manager!");
        printOutput();
    }

    @FXML
    private void handleSendCommand(ActionEvent event) {
        input = commandInput.getText();
        if (input!=null && !input.isBlank()) {
            outputArea.appendText("=> "+input + "\n");
            commandInput.clear();
            input = input.trim();
            ClientConsole.add(input);
            Handler.getInstance().run();
            Thread printThread = new Thread(()->{ // TODO thread to long output
                try{
                    printOutput();
                } catch (Exception e){
                    Thread.currentThread().interrupt();
                }
            });
            printThread.start();
            try {
                printThread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            // printOutput();
            // outputArea.appendText(outputDeque.poll());
        } else
            input = null;

    }

    private void printOutput() {
        while (!outputDeque.isEmpty()) {
            String out=outputDeque.poll();
            try {
                outputArea.appendText(out);
            } catch (NullPointerException e) {
                e.printStackTrace();
                StdConsole.writeln(e.toString());
                outputDeque.push(out);
                Alert msg = new Alert(AlertType.INFORMATION);
                msg.setContentText(out);
                msg.setTitle("Info");
                msg.show();
                StdConsole.write(out);
                e.printStackTrace();
            }
            // writeln(outputDeque.poll());
        }
    }

    public String readInput() {
        String input_read = input;
        input = null;
        return input_read;
    }

    public static void write(String output) {
        outputDeque.push(output);
        
    }

    public static void writeln(String output) {
        write(output + "\n");
    }

    public static TerminalController getInstance() {
        return instance == null ? instance = new TerminalController() : instance;
    }
}
