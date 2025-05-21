package lab8.client;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;
import lab8.client.utils.Handler;

public class ClientMain extends Application{
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler
     * to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        Application.launch(args);

        Handler handler = new Handler();
        while (true) {
            handler.run();
        }

    }

    @Override
    public void start(Stage stage) throws Exception {
        Scene scene = new Scene(FXMLLoader.load(getClass().getResource("/fxml/login.fxml")),900,700);

        stage.setScene(scene);
        stage.setTitle("Login");
        stage.show();
    }
}
