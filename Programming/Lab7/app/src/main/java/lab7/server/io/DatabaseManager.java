package lab7.server.io;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import lab7.shared.model.Chapter;
import lab7.shared.model.Coordinates;
import lab7.shared.model.MeleeWeapon;
import lab7.shared.model.SpaceMarine;

public class DatabaseManager {
    private static final Logger logger = Logger.getLogger(DatabaseManager.class.getName());

    private final String url;
    private final String user;
    private final String password;

    public DatabaseManager(String url, String user, String password) {
        logger.info("[DATABASE] Initializing DatabaseManager with URL: " + url);
        // if (!url.startsWith("jdbc:postgresql:")) {
        //     String errorMsg = "Invalid JDBC URL: must start with jdbc:postgresql:";
        //     logger.severe(errorMsg);
        //     throw new IllegalArgumentException(errorMsg);
        // }
        this.url = url;
        this.user = user;
        this.password = password;
        logger.config("DatabaseManager initialized successfully");
    }

    private Connection getConnection() throws SQLException {
        logger.fine("Attempting to get database connection");
        Connection conn = DriverManager.getConnection(url, user, password);
        logger.fine("Database connection established successfully");
        return conn;
    }

    public Long getNextId() {
        logger.fine("Getting next ID from sequence");
        try (Connection conn = getConnection();
                PreparedStatement stmt = conn.prepareStatement("SELECT nextval('space_marine_id_seq')")) {
            ResultSet rs = stmt.executeQuery();
            if (rs.next()) {
                Long id = rs.getLong(1);
                logger.fine("Retrieved next ID from sequence: " + id);
                return id;
            }
            String errorMsg = "Failed to retrieve next ID from space_marine_id_seq";
            logger.severe(errorMsg);
            throw new SQLException(errorMsg);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error getting next ID", e);
            return null;
        }
    }

    public void write(Collection<SpaceMarine> spaceMarines) throws SQLException {
        try (Connection conn = getConnection()) {
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
                        "INSERT INTO space_marine (id, name, coordinates_id, creation_date, health, loyal, achievements, melee_weapon, chapter_id) "
                                +
                                "VALUES (?, ?, ?, ?, ?, ?, ?, ?::melee_weapon, ?)");

                for (SpaceMarine marine : spaceMarines) {
                    // Validate non-null coordinates
                    if (marine.getCoordinates() == null) {
                        throw new IllegalArgumentException("SpaceMarine must have non-null coordinates");
                    }

                    // Assign creationDate and id if not set
                    if (marine.getCreationDate() == null) {
                        marine.setCreationDate(new Date());
                    }
                    if (marine.getId() == null) {
                        marine.setId(getNextId());
                    }

                    // Insert Coordinates
                    Coordinates coords = marine.getCoordinates();
                    // if (coords.getX() == null || coords.getY() == null) {
                    //     throw new IllegalArgumentException("Coordinates x and y must not be null");
                    // }
                    System.out.println("Inserting Coordinates: x=" + coords.getX() + ", y=" + coords.getY());
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

                    // Validate health
                    Double health = marine.getHealth();
                    if (health != null && health <= 0) {
                        throw new IllegalArgumentException("Health must be positive if not null, got: " + health);
                    }
                    System.out.println("Inserting SpaceMarine: id=" + marine.getId() + ", health=" + health);

                    // Insert SpaceMarine
                    marineStmt.setLong(1, marine.getId());
                    marineStmt.setString(2, marine.getName());
                    marineStmt.setInt(3, coordId);
                    marineStmt.setTimestamp(4, new Timestamp(marine.getCreationDate().getTime()));
                    if (health != null) {
                        marineStmt.setDouble(5, health);
                    } else {
                        marineStmt.setNull(5, java.sql.Types.DOUBLE);
                    }
                    if (marine.getLoyal() != null) {
                        marineStmt.setBoolean(6, marine.getLoyal());
                    } else {
                        marineStmt.setNull(6, java.sql.Types.BOOLEAN);
                    }
                    marineStmt.setString(7, marine.getAchievements());
                    marineStmt.setString(8, marine.getMeleeWeapon() != null ? marine.getMeleeWeapon().name() : null);
                    marineStmt.setObject(9, chapterId, java.sql.Types.INTEGER);
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

        try (Connection conn = getConnection()) {
            String query = "SELECT sm.*, c.x, c.y, ch.name AS chapter_name, ch.world " +
                    "FROM space_marine sm " +
                    "JOIN coordinates c ON sm.coordinates_id = c.id " +
                    "LEFT JOIN chapter ch ON sm.chapter_id = ch.id";

            try (PreparedStatement stmt = conn.prepareStatement(query)) {
                ResultSet rs = stmt.executeQuery();

                while (rs.next()) {
                    // Create Coordinates
                    Coordinates coordinates = new Coordinates(
                            rs.getDouble("x"),
                            rs.getFloat("y"));

                    // Create Chapter (if not null)
                    Chapter chapter = null;
                    if (rs.getString("chapter_name") != null) {
                        chapter = new Chapter(
                                rs.getString("chapter_name"),
                                rs.getString("world"));
                    }

                    // Create SpaceMarine
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

                    spaceMarines.add(marine);
                }
            }
        } catch (Exception e) {
            spaceMarines = new ArrayList<SpaceMarine>();
            e.printStackTrace();
        }

        return spaceMarines;
    }

}