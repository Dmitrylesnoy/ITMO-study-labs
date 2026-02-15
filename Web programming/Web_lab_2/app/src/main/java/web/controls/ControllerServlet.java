package web.controls;

import java.io.IOException;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet("/controller")
public class ControllerServlet extends HttpServlet {
    private final List<String> REQ_AREA_CHECK_PARAMS = Arrays.asList("x","y","r");
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws IOException, ServletException {
        
        Enumeration<String> attributes = request.getParameterNames();
        while (attributes.hasMoreElements()) {
            if (!(REQ_AREA_CHECK_PARAMS.contains(attributes.nextElement()))) {
                response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Missing parameters");
                return;
            }
        }
        
        ServletContext servletContext = getServletContext();
        RequestDispatcher requestDispatcher = servletContext.getRequestDispatcher("/check");
        requestDispatcher.forward(request, response);
    }
}
