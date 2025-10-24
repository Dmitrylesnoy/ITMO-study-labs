package web;

import java.util.HashMap;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;

public record Params(int x, float y, int r) {
    public static Params fromQuery(String query) throws InvalidPropertiesFormatException {
        if (query == null || query.isBlank())
            throw new InvalidPropertiesFormatException("Missing query arguments");

        var params = splitQuery(query);

        validate(params);

        int x = Integer.parseInt(params.get("x"));
        float y = Float.parseFloat(params.get("y"));
        int r = Integer.parseInt(params.get("r"));

        return new Params(x, y, r);
    }

    private static Map<String, String> splitQuery(String query) {
        Map<String, String> params = new HashMap<>();
        for (String pair : query.split("&")) {
            var p = pair.split("=");
            if (p.length == 2) {
                params.put(p[0], p[1]);
            }
        }
        return params;
    }

    private static void validate(Map<String, String> params) throws InvalidPropertiesFormatException {
        String x = params.get("x");
        if (x == null || x.isBlank())
            throw new InvalidPropertiesFormatException("x is invalid");
        try {
            int x_i = Integer.parseInt(x);
            if (x_i > 5 || x_i < -3)
                throw new InvalidPropertiesFormatException("x out of range");
        } catch (NumberFormatException e) {
            throw new InvalidPropertiesFormatException("x data format is incorrect");
        }

        String y = params.get("y");
        if (y == null || y.isBlank())
            throw new InvalidPropertiesFormatException("y is invalid");
        try {
            float y_f = Float.parseFloat(y);
            if (y_f > 3 || y_f < -3)
                throw new InvalidPropertiesFormatException("y out of range");
        } catch (NumberFormatException e) {
            throw new InvalidPropertiesFormatException("y data format is incorrect");
        }

        String r = params.get("r");
        if (r == null || r.isBlank())
            throw new InvalidPropertiesFormatException("r is invalid");
        try {
            int r_i = Integer.parseInt(r);
            if (r_i > 5 || r_i < 1)
                throw new InvalidPropertiesFormatException("r out of range");
        } catch (NumberFormatException e) {
            throw new InvalidPropertiesFormatException("r data format is incorrect");
        }
    }
}
