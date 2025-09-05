package lab7.server.io.database;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class UsersDatabase {

    private String hashPassword(String password) throws NoSuchAlgorithmException {
        if (password == null) {
            return null;
        }

        MessageDigest md = MessageDigest.getInstance(   "SHA-1");
        byte[] hashBytes = md.digest(password.getBytes());

        StringBuilder sb = new StringBuilder();
        for (byte b : hashBytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    public Integer getUserId(String name, String password) throws SQLException, NoSuchAlgorithmException {
        if (name == null || password == null) {
            throw new IllegalArgumentException("Name and password must not be null");
        }

        String hashedPassword = hashPassword(password);

        try (Connection conn = DatabaseManager.getConnection();
                PreparedStatement stmt = conn.prepareStatement(
                        "SELECT id FROM users WHERE name = ? AND password = ?")) {
            stmt.setString(1, name);
            stmt.setString(2, hashedPassword);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getInt("id");
            }
            return null;
        }
    }

    public Integer addUser(String name, String password) throws SQLException, NoSuchAlgorithmException {
        if (name == null || password == null) {
            throw new IllegalArgumentException("Name and password must not be null");
        }

        // String hashedPassword = hashPassword(password);
        String hashedPassword = password;

        try (Connection conn = DatabaseManager.getConnection()) {
            conn.setAutoCommit(false);
            try {
                try (PreparedStatement checkStmt = conn.prepareStatement(
                        "SELECT id FROM users WHERE name = ? AND password = ?")) {
                    checkStmt.setString(1, name);
                    checkStmt.setString(2, hashedPassword);
                    ResultSet rs = checkStmt.executeQuery();
                    if (rs.next()) {
                        conn.commit();
                        return rs.getInt("id"); 
                    }
                }

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
                    insertStmt.setString(2, hashedPassword);
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