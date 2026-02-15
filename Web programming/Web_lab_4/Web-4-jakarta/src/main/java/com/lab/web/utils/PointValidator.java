package com.lab.web.utils;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.lab.web.data.PointData;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PointValidator implements Serializable {

    public static boolean validateX(Float x) {
        return x >= -3 && x <= 5;
    }

    public static boolean validateY(Float y) {
        return y >= -5 && y <= 5;
    }

    public static boolean validateR(Float r) {
        return r >= 0 && r <= 5;
    }

    public static PointData fillPoint(String xStr, String yStr, String rStr, boolean validate)
            throws IllegalArgumentException {
        long start = System.nanoTime();

        Float x = Float.valueOf(xStr);
        Float y = Float.valueOf(yStr);
        Float r = Float.valueOf(rStr);

        if (validate && !(validateX(x) && validateY(y) && validateR(r)))
            throw new IllegalArgumentException("Point coordinates out of diaposones");

        PointData point = new PointData(x, y, r);
        point.setHit(checkArea(x, y, r));
        point.setDate(LocalDateTime.now());
        point.setExecTime(System.nanoTime() - start);

        return point;
    }

    public static boolean checkArea(float x, float y, float r) {
        if (x == 0 && y == 0)
            return true;
        if (x >= 0 && y >= 0) { // Quadrant 1:
            return false; // nothing

        } else if (x >= 0 && y <= 0) { // Quadrant 4:
            return x <= r && y >= -1 * r; // Rectangle

        } else if (x <= 0 && y >= 0) { // Quadrant 2:
            return x * x + y * y <= r * r; // Quarter circle

        } else { // Quadrant 3:
            return y >= -0.5 * x - 0.5 * r; // Triangle
        }
    }
}
