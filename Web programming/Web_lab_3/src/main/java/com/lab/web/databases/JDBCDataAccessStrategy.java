package com.lab.web.databases;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import com.lab.web.data.PointData;

import jakarta.transaction.Transactional;

@Transactional
public class JDBCDataAccessStrategy implements DataAccessStrategy {
    private DataSource dataSource;

    public JDBCDataAccessStrategy() {
        try {
            InitialContext ctx = new InitialContext();
            this.dataSource = (DataSource) ctx.lookup("java:/jboss/datasources/MyPostgresDS");
        } catch (NamingException e) {
            throw new RuntimeException("Failed to lookup DataSource", e);
        }
    }

    @Override
    public List<PointData> getAllPoints() {
        List<PointData> points = new ArrayList<>();
        String sql = "SELECT id, x, y, r, hit, exec_time, date FROM point_data ORDER BY date DESC";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql);
                ResultSet rs = stmt.executeQuery()) {
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
        System.out.println("JDBC: point added");
        String sql = "INSERT INTO point_data (x, y, r, hit, exec_time, date) VALUES (?, ?, ?, ?, ?, ?)";
        try (Connection conn = dataSource.getConnection();
                PreparedStatement stmt = conn.prepareStatement(sql)) {
            stmt.setFloat(1, point.getX());
            stmt.setFloat(2, point.getY());
            stmt.setFloat(3, point.getR());
            stmt.setBoolean(4, point.isHit());
            stmt.setLong(5, point.getExecTime());
            stmt.setTimestamp(6, Timestamp.valueOf(point.getDate()));
            stmt.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to add point", e);
        }
    }
}
