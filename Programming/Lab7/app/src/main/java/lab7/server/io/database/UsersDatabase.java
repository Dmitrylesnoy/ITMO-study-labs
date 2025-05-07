package lab7.server.io.database;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class UsersDatabase {

    public Integer getUserId(String name, String password) throws SQLException {
        if (name == null || password == null) {
            throw new IllegalArgumentException("Name and password must not be null");
        }

        try (Connection conn = DatabaseManager.getConnection();
                PreparedStatement stmt = conn.prepareStatement(
                        "SELECT id FROM users WHERE name = ? AND password = ?")) {
            stmt.setString(1, name);
            stmt.setString(2, password);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getInt("id");
            }
            return null;
        // } catch (SQLException e) {
        //     e.printStackTrace();
        //     return null;
        }
    }

    public Integer addUser(String name, String password) throws SQLException {
        if (name == null || password == null) {
            throw new IllegalArgumentException("Name and password must not be null");
        }

        try (Connection conn = DatabaseManager.getConnection()) {
            conn.setAutoCommit(false);
            try {
                // Check if user exists with matching name and password
                try (PreparedStatement checkStmt = conn.prepareStatement(
                        "SELECT id FROM users WHERE name = ? AND password = ?")) {
                    checkStmt.setString(1, name);
                    checkStmt.setString(2, password);
                    ResultSet rs = checkStmt.executeQuery();
                    if (rs.next()) {
                        conn.commit();
                        return rs.getInt("id"); // Return existing user's ID
                    }
                }

                // Check if name exists with different password
                try (PreparedStatement checkNameStmt = conn.prepareStatement(
                        "SELECT id FROM users WHERE name = ?")) {
                    checkNameStmt.setString(1, name);
                    ResultSet rs = checkNameStmt.executeQuery();
                    if (rs.next()) {
                        conn.rollback();
                        throw new IllegalArgumentException("Username exists with a different password");
                    }
                }

                // Insert new user
                try (PreparedStatement insertStmt = conn.prepareStatement(
                        "INSERT INTO users (name, password) VALUES (?, ?) RETURNING id")) {
                    insertStmt.setString(1, name);
                    insertStmt.setString(2, password);
                    ResultSet rs = insertStmt.executeQuery();
                    if (rs.next()) {
                        Integer userId = rs.getInt("id");
                        conn.commit();
                        return userId;
                    }
                    throw new SQLException("Failed to retrieve new user ID");
                }
            } catch (SQLException | IllegalArgumentException e) {
                conn.rollback();
                throw e;
            } finally {
                conn.setAutoCommit(true);
            }
        }
    }
}
