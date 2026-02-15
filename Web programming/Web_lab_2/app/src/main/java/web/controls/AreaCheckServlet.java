package web.controls;

import java.io.IOException;
import java.time.LocalDateTime;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import web.model.HitData;
import web.model.Point;

@WebServlet("/check")
public class AreaCheckServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // String xStr = request.getParameter("x");
        String[] xList = request.getParameterValues("x");
        String yStr = request.getParameter("y");
        String rStr = request.getParameter("r");

        if (xList == null || yStr == null || rStr == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Missing parameters");
            return;
        }

        try {
            HitData hitData = (HitData) request.getSession().getAttribute("hitData");
            if (hitData == null) {
                hitData = new HitData();
                request.getSession().setAttribute("hitData", hitData);
            }
            for (String xStr : xList) {
                long startTime = System.nanoTime();

                float x = validateX(xStr);
                float y = validateY(yStr);
                int r = validateR(rStr);
                request.getSession().setAttribute("lastR", r);

                boolean hit = checkHit(x, y, r);
                float execTime = (System.nanoTime() - startTime) / 1000.0f;
                Point point = new Point(x, y, r, hit, execTime, LocalDateTime.now());

                hitData.addPoint(point);
            }

            RequestDispatcher dispatcher = request.getRequestDispatcher("/results.jsp");
            dispatcher.forward(request, response);
        } catch (NumberFormatException e) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Invalid parameters.\n" + e.toString());
        }
    }

    private boolean checkHit(float x, float y, int r) {
        if (x >= 0 && y >= 0) {
            // Quadrant 1: Quarter circle
            return x * x + y * y <= r * r;
        } else if (x >= 0 && y <= 0) {
            // Quadrant 4: Triangle
            return x - y <= r / 2.0;
        } else if (x <= 0 && y >= 0) {
            // Quadrant 2: nothing
            return false;
        } else {
            // Quadrant 3: Rectangle
            return x >= -r && y >= -r / 2.0;
        }
    }

    private Float validateX(String x) throws NumberFormatException {
        if (x == null || x.isBlank())
            throw new NumberFormatException("x is invalid");
        float x_f = Float.parseFloat(x);
        if (x_f > 3 || x_f < -5)
            throw new NumberFormatException("x out of range");
        return x_f;
    }

    private Float validateY(String y) throws NumberFormatException {

        if (y == null || y.isBlank())
            throw new NumberFormatException("y is invalid");
        float y_f = Float.parseFloat(y);
        if (y_f > 3 || y_f < -5)
            throw new NumberFormatException("y out of range");
        return y_f;
    }

    private Integer validateR(String r) throws NumberFormatException {
        if (r == null || r.isBlank())
            throw new NumberFormatException("r is invalid");
        int r_i = Integer.parseInt(r);
        if (r_i > 5 || r_i < 1)
            throw new NumberFormatException("r out of range");
        return r_i;
    }
}
