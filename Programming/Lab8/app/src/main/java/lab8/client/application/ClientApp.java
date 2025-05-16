package lab8.client.application;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import lab8.shared.messages.User;

import java.io.IOException;

public class ClientApp extends Application {
    private Stage primaryStage;
    private AuthController authController;
    private MainController mainController;
    private User currentUser;

    @Override
    public void start(Stage primaryStage) throws Exception {
        this.primaryStage = primaryStage;
        showAuthWindow();
    }

    public void showAuthWindow() throws IOException {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("AuthWindow.fxml"));
        Parent root = loader.load();

        authController = loader.getController();
        authController.setStage(primaryStage);
        authController.setClientApp(this);

        primaryStage.setTitle("Авторизация");
        primaryStage.setScene(new Scene(root));
        primaryStage.show();
    }

    public void showMainWindow() throws IOException {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("MainWindow.fxml"));
        Parent root = loader.load();

        mainController = loader.getController();
        mainController.setClientApp(this);
        mainController.setCurrentUser(currentUser);

        // Загружаем начальные данные
        loadInitialData();

        Stage mainStage = new Stage();
        mainStage.setTitle("Клиентское приложение - " + currentUser.login());
        mainStage.setScene(new Scene(root, 1000, 700));
        mainStage.show();
    }

    public boolean authenticate(String login, String password) {
        // Реализация аутентификации через сервер
        // Временная заглушка
        this.currentUser = new User(login, password);
        return true;
    }

    public boolean register(String login, String password) {
        // Реализация регистрации через сервер
        // Временная заглушка
        return true;
    }

    public void logout() {
        currentUser = null;
        primaryStage.close();
        try {
            showAuthWindow();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void loadInitialData() {
        // Загрузка данных с сервера
        // Временная заглушка
        // mainController.updateCollection(...);
    }

    public boolean removeObject(int id) {
        // Отправка запроса на удаление на сервер
        // Временная заглушка
        return true;
    }

    public String executeCommand(String command) {
        // Отправка команды на сервер
        // Временная заглушка
        return "Команда выполнена: " + command;
    }

    public static void main(String[] args) {
        launch(args);
    }
}