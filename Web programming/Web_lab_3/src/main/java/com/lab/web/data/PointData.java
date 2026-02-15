package com.lab.web.data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "point_data")
public class PointData implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private float x;
    private float y;
    private float r;
    private boolean hit;

    @Column(name = "exec_time")
    private long execTime;
    private LocalDateTime date;

    @Override
    public String toString() {
        return String.format("X: %f , Y: %f , R: %f , Hit: %b , Exec: %d , Date: %s", x, y, r, hit, execTime, date);
    }

    public String getDataFormatted() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        return date.format(formatter);
    }
}
