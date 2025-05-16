package lab8.client;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import lab8.client.utils.Handler;

public class ClientMain extends Application {
    /**
     * The main function creates two SpaceMarine objects and initializes a Handler
     * to run a console-based program indefinitely.
     */
    public static void main(String[] args) {
        // Application.launch(args);

        Handler handler = new Handler();
        while (true) {
            handler.run();
        }
    }

    @Override
    public void start(Stage stage) throws Exception {

        // установка надписи
        Text text = new Text("Hello METANIT.COM!");
        text.setLayoutY(80); // установка положения надписи по оси Y
        text.setLayoutX(80); // установка положения надписи по оси X
        Group root = new Group(text);

        // Parent root = FXMLLoader.load(getClass().getResource("main.fxml"));
        Scene scene = new Scene(root);

        stage.setScene(scene);

        stage.setTitle("Hello JavaFX");
        stage.setWidth(250);
        stage.setHeight(200);

        stage.show();
    }
}
