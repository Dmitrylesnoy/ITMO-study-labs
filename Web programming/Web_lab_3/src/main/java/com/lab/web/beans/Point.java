package com.lab.web.beans;

import java.io.Serializable;

import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Named;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@Named("point")
@SessionScoped
public class Point implements Serializable {
    private float x;
    private float y;
    private float r;
    private boolean isGraphClick = false;

    public void updateValue() {
        r = (r % 5) + 1;
        System.out.println("New R value setted: " + r);
    }

    public boolean getGraphClick() {
        return isGraphClick;
    }

    public void setGraphClick(boolean graphClick) {
        isGraphClick = graphClick;
    }

    public void reset() {
        this.r = 0;
    }
}
