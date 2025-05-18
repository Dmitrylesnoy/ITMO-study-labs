package lab8.client;

import java.io.IOException;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import lab8.client.application.LoginController;
import lab8.client.utils.Handler;

public class ClientMain extends Application{
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler
     * to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        Application.launch(args);

            // Handler handler = new Handler();
            // while (true) {
            //     handler.run();
            // }

    }

    @Override
    public void start(Stage stage) throws Exception {
        VBox root = FXMLLoader.load(getClass().getResource("application/LoginWindow.fxml"));
        Scene scene = new Scene(root);

        stage.setScene(scene);
        stage.setTitle("Login");
        stage.setWidth(900);
        stage.setHeight(700);
        stage.show();
    }
}
