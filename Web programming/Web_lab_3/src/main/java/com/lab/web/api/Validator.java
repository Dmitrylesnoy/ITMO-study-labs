package com.lab.web.api;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.lab.web.beans.Point;
import com.lab.web.data.PointData;

import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIInput;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import lombok.Getter;
import lombok.Setter;

@Gettedeskr
@Setter
@Named("validator")
@RequestScoped
public class Validator implements Serializable {
    @Inject
    private Point point;

    private float x;
    private float y;
    private float r;

    public void validateX(FacesContext context, UIComponent comp, Object value) {
        String graphClickParam = context.getExternalContext().getRequestParameterMap().get("formCoords:graphClickFlag");
        if ("true".equals(graphClickParam)) {
            ((UIInput) comp).setValid(true);
            return;
        }

        float inpX = (float) value;
        System.out.println("Inside X validator. Value: " + inpX);

        if (-4 > inpX || inpX > 4) {
            ((UIInput) comp).setValid(false);
            FacesMessage message = new FacesMessage("X value must be integer, from -4 to 4");
            context.addMessage(comp.getClientId(context), message);
            System.out.println("Validation failed!");
        } else {
            ((UIInput) comp).setValid(true);
            System.out.println("Validation Sucess!");
        }
    }

    public void validateY(FacesContext context, UIComponent comp, Object value) {
        String graphClickParam = context.getExternalContext().getRequestParameterMap().get("formCoords:graphClickFlag");
        if ("true".equals(graphClickParam)) {
            ((UIInput) comp).setValid(true);
            return;
        }

        float inpY = (float) value;
        System.out.println("Inside Y validator. Value: " + inpY);

        if (-5 > inpY || inpY > 3) {
            ((UIInput) comp).setValid(false);
            FacesMessage message = new FacesMessage("Y value must be number, from -5 to 3");
            context.addMessage(comp.getClientId(context), message);
            System.out.println("Validation failed!");
        } else {
            ((UIInput) comp).setValid(true);
            System.out.println("Validation Sucess!");
        }
    }

    public void validateR(FacesContext context, UIComponent comp, Object value) {
        float inpR = (float) value;
        System.out.println("Inside R validator. Value: " + inpR);

        if (inpR <= 0) {
            ((UIInput) comp).setValid(false);
            FacesMessage message = new FacesMessage("R value must be selected and not be zero");
            context.addMessage(comp.getClientId(context), message);
            System.out.println("R validation failed!");
        } else {
            ((UIInput) comp).setValid(true);
            System.out.println("R validation Success!");
        }
    }

    public static PointData fillPoint(PointData point) {
        long start = System.nanoTime();
        point.setHit(checkArea(point.getX(), point.getY(), point.getR()));
        point.setDate(LocalDateTime.now());
        point.setExecTime(System.nanoTime() - start);

        return point;
    }

    public static boolean checkArea(float x, float y, float r) {
        if (x == 0 && y == 0)
            return true;
        if (x > 0 && y > 0) { // Quadrant 1:
            return x <= r && y <= r / 2.0; // Rectangle

        } else if (x > 0 && y < 0) { // Quadrant 4:
            return false; // nothing

        } else if (x < 0 && y > 0) { // Quadrant 2:
            return y - x <= r; // Triangle

        } else { // Quadrant 3:
            return x * x + y * y <= (r / 2.0) * (r / 2.0); // Quarter circle
        }
    }
}
