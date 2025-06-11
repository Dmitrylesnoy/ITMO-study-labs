package lab8.client;

import javafx.application.Application;
import javafx.stage.Stage;
import lab8.client.controllers.LoginController;
import lab8.client.controllers.ToolbarController;
import lab8.client.utils.Handler;
import lab8.shared.io.console.ClientConsole;

import java.io.*;

public class ClientMain extends Application {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler
     * to run a console-based program indefinitely.
     */
    public static void main(String[] args) throws IOException {
       launch(args);
       ClientConsole.getInstance().writeln("Application statred");

    //    Handler handler = new Handler();
    //    while (true) {
    //        handler.run();
    //    }

    }

    @Override
    public void start(Stage stage) throws Exception {
        // Handler handler = new Handler();
        // handler.run();
        Handler.getInstance();
        ToolbarController.getInstance();
        LoginController loginController = new LoginController();
        loginController.open().show();
        // stageL.show();
    }
}
