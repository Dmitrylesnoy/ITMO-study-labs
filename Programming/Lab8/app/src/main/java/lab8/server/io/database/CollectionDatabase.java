package lab8.server.io.database;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import lab8.shared.model.Chapter;
import lab8.shared.model.Coordinates;
import lab8.shared.model.MeleeWeapon;
import lab8.shared.model.SpaceMarine;

public class CollectionDatabase {
    public Long getNextId() throws SQLException {
        try (Connection conn = DatabaseManager.getConnection();
                PreparedStatement stmt = conn.prepareStatement("SELECT nextval('space_marine_id_seq')")) {
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getLong(1);
            }
            throw new SQLException("Failed to retrieve next ID from space_marine_id_seq");
        }
    }

    public void write(Collection<SpaceMarine> spaceMarines) throws SQLException {
        try (Connection conn = DatabaseManager.getConnection()) {
            conn.setAutoCommit(false);
            try {
                // Clear existing data
                try (PreparedStatement clearStmt = conn.prepareStatement(
                        "DELETE FROM space_marine; DELETE FROM coordinates; DELETE FROM chapter")) {
                    clearStmt.executeUpdate();
                }

                // Prepare statements
                PreparedStatement coordStmt = conn.prepareStatement(
                        "INSERT INTO coordinates (x, y) VALUES (?, ?) RETURNING id");
                PreparedStatement chapterStmt = conn.prepareStatement(
                        "INSERT INTO chapter (name, world) VALUES (?, ?) RETURNING id");
                PreparedStatement marineStmt = conn.prepareStatement(
                        "INSERT INTO space_marine (id, name, coordinates_id, creation_date, health, loyal, achievements, melee_weapon, chapter_id, creator_id) "
                                +
                                "VALUES (?, ?, ?, ?, ?, ?, ?, ?::melee_weapon, ?, ?)");

                for (SpaceMarine marine : spaceMarines) {
                    if (marine.getCreationDate() == null) {
                        marine.setCreationDate(new Date());
                    }
                    if (marine.getId() == null) {
                        marine.setId(getNextId());
                    }

                    // Insert Coordinates
                    Coordinates coords = marine.getCoordinates();
                    // System.out.println("Inserting Coordinates: x=" + coords.getX() + ", y=" + coords.getY());
                    coordStmt.setDouble(1, coords.getX());
                    coordStmt.setFloat(2, coords.getY());
                    ResultSet coordRs = coordStmt.executeQuery();
                    if (!coordRs.next()) {
                        throw new SQLException("Failed to retrieve coordinates ID");
                    }
                    int coordId = coordRs.getInt(1);
                    if (coordId == -1) {
                        throw new SQLException("Invalid coordinates ID returned");
                    }

                    // Insert Chapter (if not null)
                    Chapter chapter = marine.getChapter();
                    Integer chapterId = null;
                    if (chapter != null) {
                        chapterStmt.setString(1, chapter.getName());
                        chapterStmt.setString(2, chapter.getWorld());
                        ResultSet chapterRs = chapterStmt.executeQuery();
                        if (!chapterRs.next()) {
                            throw new SQLException("Failed to retrieve chapter ID");
                        }
                        chapterId = chapterRs.getInt(1);
                    }

                    // Insert SpaceMarine
                    marineStmt.setLong(1, marine.getId());
                    marineStmt.setString(2, marine.getName());
                    marineStmt.setInt(3, coordId);
                    marineStmt.setTimestamp(4, new Timestamp(marine.getCreationDate().getTime()));
                    marineStmt.setObject(5, marine.getHealth(), java.sql.Types.DOUBLE);
                    if (marine.getLoyal() != null) {
                        marineStmt.setBoolean(6, marine.getLoyal());
                    } else {
                        marineStmt.setNull(6, java.sql.Types.BOOLEAN);
                    }
                    marineStmt.setString(7, marine.getAchievements());
                    marineStmt.setString(8, marine.getMeleeWeapon() != null ? marine.getMeleeWeapon().name() : null);
                    marineStmt.setObject(9, chapterId, java.sql.Types.INTEGER);
                    marineStmt.setInt(10, marine.getCreatorId());
                    marineStmt.executeUpdate();
                }

                conn.commit();
            } catch (SQLException e) {
                conn.rollback();
                throw e;
            } finally {
                conn.setAutoCommit(true);
            }
        }
    }

    public Collection<SpaceMarine> read() {
        Collection<SpaceMarine> spaceMarines = new ArrayList<>();

        try (Connection conn = DatabaseManager.getConnection()) {
            String query = "SELECT sm.*, c.x, c.y, ch.name AS chapter_name, ch.world " +
                    "FROM space_marine sm " +
                    "JOIN coordinates c ON sm.coordinates_id = c.id " +
                    "LEFT JOIN chapter ch ON sm.chapter_id = ch.id";

            try (PreparedStatement stmt = conn.prepareStatement(query)) {
                ResultSet rs = stmt.executeQuery();

                while (rs.next()) {
                    Coordinates coordinates = new Coordinates(
                            rs.getDouble("x"),
                            rs.getFloat("y"));

                    Chapter chapter = null;
                    if (rs.getString("chapter_name") != null) {
                        chapter = new Chapter(
                                rs.getString("chapter_name"),
                                rs.getString("world"));
                    }

                    Double health = rs.getDouble("health");
                    if (rs.wasNull())
                        health = null;
                    Boolean loyal = rs.getBoolean("loyal");
                    if (rs.wasNull())
                        loyal = null;
                    String weapon = rs.getString("melee_weapon");

                    SpaceMarine marine = new SpaceMarine(
                            rs.getString("name"),
                            coordinates,
                            health,
                            loyal,
                            rs.getString("achievements"),
                            weapon != null ? MeleeWeapon.valueOf(weapon) : null,
                            chapter);
                    marine.setId(rs.getLong("id"));
                    marine.setCreationDate(new Date(rs.getTimestamp("creation_date").getTime()));
                    marine.setCreatorId(rs.getInt("creator_id"));

                    spaceMarines.add(marine);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            spaceMarines = new ArrayList<SpaceMarine>();
        }

        return spaceMarines;
    }

    public void addSpaceMarine(SpaceMarine marine) throws SQLException {
        try (Connection conn = DatabaseManager.getConnection()) {
            conn.setAutoCommit(false);
            try {
                if (marine.getId() == null) {
                    marine.setId(getNextId());
                }
                if (marine.getCreationDate() == null) {
                    marine.setCreationDate(new Date());
                }

                PreparedStatement coordStmt = conn.prepareStatement(
                        "INSERT INTO coordinates (x, y) VALUES (?, ?) RETURNING id");
                Coordinates coords = marine.getCoordinates();
                coordStmt.setDouble(1, coords.getX());
                coordStmt.setFloat(2, coords.getY());
                ResultSet coordRs = coordStmt.executeQuery();
                if (!coordRs.next()) {
                    throw new SQLException("Failed to retrieve coordinates ID");
                }
                int coordId = coordRs.getInt(1);

                // Insert Chapter (if not null)
                PreparedStatement chapterStmt = conn.prepareStatement(
                        "INSERT INTO chapter (name, world) VALUES (?, ?) RETURNING id");
                Chapter chapter = marine.getChapter();
                Integer chapterId = null;
                if (chapter != null) {
                    chapterStmt.setString(1, chapter.getName());
                    chapterStmt.setString(2, chapter.getWorld());
                    ResultSet chapterRs = chapterStmt.executeQuery();
                    if (!chapterRs.next()) {
                        throw new SQLException("Failed to retrieve chapter ID");
                    }
                    chapterId = chapterRs.getInt(1);
                }

                // Insert SpaceMarine
                PreparedStatement marineStmt = conn.prepareStatement(
                        "INSERT INTO space_marine (id, name, coordinates_id, creation_date, health, loyal, achievements, melee_weapon, chapter_id, creator_id) "
                                +
                                "VALUES (?, ?, ?, ?, ?, ?, ?, ?::melee_weapon, ?, ?)");

                marineStmt.setLong(1, marine.getId());
                marineStmt.setString(2, marine.getName());
                marineStmt.setInt(3, coordId);
                marineStmt.setTimestamp(4, new Timestamp(marine.getCreationDate().getTime()));
                marineStmt.setObject(5, marine.getHealth(), java.sql.Types.DOUBLE);
                if (marine.getLoyal() != null) {
                    marineStmt.setBoolean(6, marine.getLoyal());
                } else {
                    marineStmt.setNull(6, java.sql.Types.BOOLEAN);
                }
                marineStmt.setString(7, marine.getAchievements());
                marineStmt.setString(8, marine.getMeleeWeapon() != null ? marine.getMeleeWeapon().name() : null);
                marineStmt.setObject(9, chapterId, java.sql.Types.INTEGER);
                marineStmt.setInt(10, marine.getCreatorId());

                marineStmt.executeUpdate();
                conn.commit();
            } catch (SQLException e) {
                conn.rollback();
                throw e;
            } finally {
                conn.setAutoCommit(true);
            }
        }
    }
}