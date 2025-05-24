package lab8.client;

import javafx.application.Application;
import javafx.stage.Stage;
import lab8.client.controllers.LoginController;
import lab8.client.controllers.ToolbarController;
import lab8.client.utils.Handler;

public class ClientMain extends Application {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler
     * to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        try {
            Application.launch(args);

            Handler handler = new Handler();
            while (true) {
                handler.run();
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void start(Stage stage) throws Exception {
        ToolbarController.getInstance();
        LoginController loginController = new LoginController();
        loginController.open().show();
        // stageL.show();
    }
}
