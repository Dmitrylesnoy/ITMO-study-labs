package com.lab.web.data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.fasterxml.jackson.annotation.JsonIgnore;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "point_data")
@NoArgsConstructor
public class PointData implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @JsonIgnore
    private Long id;

    private float x;
    private float y;
    private float r;
    private boolean hit;

    @Column(name = "exec_time")
    private long execTime;
    @JsonIgnore
    private LocalDateTime date;
    @Column(name = "user_id")
    @JsonIgnore
    private Long user;

    public PointData(float x, float y, float r) {
        this.x = x;
        this.y = y;
        this.r = r;
    }

    public PointData(float x, float y, float r, boolean hit, long execTime, LocalDateTime date, Long user) {
        this.x = x;
        this.y = y;
        this.r = r;
        this.hit = hit;
        this.execTime = execTime;
        this.date = date;
        this.user = user;
    }

    @Override
    public String toString() {
        return String.format("X: %f , Y: %f , R: %f , Hit: %b , Exec: %d , Date: %s , UserID: %d", x, y, r, hit,
                execTime, date, user);
    }

    public String getdateFormatted() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        return date.format(formatter);
    }
}
