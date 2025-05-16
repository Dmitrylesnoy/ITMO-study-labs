package lab8.server.io.database;

import java.security.NoSuchAlgorithmException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Collection;

import lab8.shared.model.SpaceMarine;

public class DatabaseManager {
    private static String url;
    private static String user;
    private static String password;

    private UsersDatabase userdb = new UsersDatabase();
    private CollectionDatabase collectiondb = new CollectionDatabase();

    public DatabaseManager(String url, String user, String password) {
        if (!url.startsWith("jdbc:postgresql:")) {
            throw new IllegalArgumentException("Invalid JDBC URL: must start with jdbc:postgresql:");
        }
        this.url = url;
        this.user = user;
        this.password = password;
    }

    public static Connection getConnection() throws SQLException {
        System.out.println("Connecting to: " + url);
        return DriverManager.getConnection(url, user, password);
    }

    public Integer getUserId(String name, String password) throws SQLException {
        try {
            return userdb.getUserId(name, password);
        } catch (NoSuchAlgorithmException e) {
            e.getStackTrace();
            return null;
        }
    }

    public Integer addUser(String name, String password) throws SQLException {
        try {
            return userdb.addUser(name, password);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            return null;
        }
    }

    public Long getNextId() throws SQLException {
        return collectiondb.getNextId();
    }

    public boolean write(Collection<SpaceMarine> spaceMarines) {
        try {
            collectiondb.write(spaceMarines);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public Collection<SpaceMarine> read() {
        return collectiondb.read();
    }

    public boolean addSpaceMarine(SpaceMarine marine) {
        try {
            collectiondb.addSpaceMarine(marine);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}