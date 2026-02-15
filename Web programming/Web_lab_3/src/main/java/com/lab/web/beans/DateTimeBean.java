package com.lab.web.beans;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;

@Named()
@ViewScoped
public class DateTimeBean implements Serializable {

    public String getStringCurrentDateTime() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        return getCurrentDateTime().format(formatter);
    }

    public LocalDateTime getCurrentDateTime() {
        return LocalDateTime.now();
    }
}
