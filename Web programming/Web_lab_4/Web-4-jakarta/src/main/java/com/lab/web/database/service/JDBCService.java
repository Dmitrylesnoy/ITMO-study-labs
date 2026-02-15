package com.lab.web.database.service;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import com.lab.web.data.PointData;
import com.lab.web.data.User;
import com.lab.web.database.repository.PointsRepository;
import com.lab.web.database.repository.UserRepository;
import com.lab.web.utils.auth.PasswordService;

import jakarta.ejb.Singleton;
import jakarta.ejb.Startup;
import jakarta.transaction.Transactional;

@Startup
@Singleton
@Transactional
public class JDBCService implements UserRepository, PointsRepository {
    private DataSource dataSource;

    private static final Logger logger = Logger.getLogger(JDBCService.class.getName());

    public JDBCService() {
        try {
            InitialContext ctx = new InitialContext();
            this.dataSource = (DataSource) ctx.lookup("java:/jboss/datasources/MyPostgresDS");
        } catch (NamingException e) {
            throw new RuntimeException("Failed to lookup DataSource", e);
        }
    }

    @Override
    public List<PointData> getAllPoints(Long userId) {
        List<PointData> points = new ArrayList<>();
        String sql = "SELECT id, x, y, r, hit, exec_time, date FROM point_data WHERE user_id = ? ORDER BY date DESC";

        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql);) {
            stmt.setLong(1, userId);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                PointData point = new PointData();
                point.setId(rs.getLong("id"));
                point.setX(rs.getFloat("x"));
                point.setY(rs.getFloat("y"));
                point.setR(rs.getFloat("r"));
                point.setHit(rs.getBoolean("hit"));
                point.setExecTime(rs.getLong("exec_time"));
                point.setDate(rs.getTimestamp("date").toLocalDateTime());
                points.add(point);
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to retrieve points", e);
        }
        return points;
    }

    @Override
    public void addPoint(PointData point) {
        logger.info("JDBC: point added");
        String sql = "INSERT INTO point_data (x, y, r, hit, exec_time, date, user_id) VALUES (?, ?, ?, ?, ?, ?, ?)";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setFloat(1, point.getX());
            stmt.setFloat(2, point.getY());
            stmt.setFloat(3, point.getR());
            stmt.setBoolean(4, point.isHit());
            stmt.setLong(5, point.getExecTime());
            stmt.setTimestamp(6, Timestamp.valueOf(point.getDate()));
            stmt.setLong(7, point.getUser());
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to add point", e);
        }
    }

    @Override
    public boolean isUserExist(User user) {
        String sql = "SELECT COUNT(*) FROM web_users WHERE username = ?";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setString(1, user.username());
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getInt(1) > 0;
            }
            return false;
        } catch (SQLException e) {
            throw new RuntimeException("Failed to check if user exists", e);
        }
    }

    @Override
    public void createUser(User user) {
        String sql = "INSERT INTO web_users (username, password) VALUES (?, ?)";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setString(1, user.username());
            stmt.setString(2, PasswordService.hashPassword(user.password()));
            stmt.executeUpdate();
            logger.info("JDBC: user created - " + user.username());
        } catch (SQLException e) {
            throw new RuntimeException("Failed to create user", e);
        }
    }

    @Override
    public boolean checkPassword(User user) {
        String sql = "SELECT password FROM web_users WHERE username = ?";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setString(1, user.username());
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                String storedPassword = rs.getString("password");
                return PasswordService.verifyPassword(user.password(), storedPassword);
            }
            return false;
        } catch (SQLException e) {
            throw new RuntimeException("Failed to check password", e);
        }
    }

    @Override
    public User getUserByUsername(String username) {
        String sql = "SELECT id, username, password FROM web_users WHERE username = ?";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setString(1, username);
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                return new User(
                        rs.getLong("id"),
                        rs.getString("username"),
                        rs.getString("password"));
            }
            return null;
        } catch (SQLException e) {
            throw new RuntimeException("Failed to get user by username", e);
        }
    }
}
