package lab8.client;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;
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
        Stage stageL = ToolbarController.getInstance().openWindow("/fxml/login.fxml","login",900,700);
        stageL.show();
    }
}
