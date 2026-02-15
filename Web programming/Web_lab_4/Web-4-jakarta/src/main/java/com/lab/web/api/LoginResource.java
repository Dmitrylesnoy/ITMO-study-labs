package com.lab.web.api;

import java.util.logging.Logger;

import com.lab.web.api.records.LoginRequest;
import com.lab.web.api.records.LoginResponse;
import com.lab.web.api.records.LogoutResponse;
import com.lab.web.data.User;
import com.lab.web.database.repository.UserRepository;
import com.lab.web.utils.RateLimiter;
import com.lab.web.utils.auth.UserVetification;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HeaderParam;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.core.UriInfo;
import jakarta.ws.rs.ext.Provider;

@Provider
@RequestScoped
@Path("/user")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class LoginResource {
    @Context
    private UriInfo context;

    @Context
    private HttpServletRequest httpRequest;

    @Inject
    private UserRepository userRepo;

    @Inject
    private UserVetification userVetification;

    private static final Logger logger = Logger.getLogger(LoginResource.class.getName());

    /*
     * input: {username, password}
     * output: {result: "successfully login" | "user registered" | "wrong password",
     * token: string | null}
     */
    @POST
    @Path("/login")
    public Response checkPassword(LoginRequest request) {
        logger.info("Login attempt for user: " + (request != null ? request.username() : "null"));

        try {

            if (!RateLimiter.tryLoginConsume(httpRequest, 1)) {
                return Response.status(Status.TOO_MANY_REQUESTS)
                        .entity(new LoginResponse("too many requests", null))
                        .build();
            }

            if (request == null || request.username() == null || request.password() == null) {
                logger.warning("Invalid login request - missing required fields. Request: " + request.toString());
                return Response.status(Status.BAD_REQUEST)
                        .entity(new LoginResponse("invalid request", null))
                        .build();
            }

            if (request.username().trim().isEmpty() || request.password().trim().isEmpty()) {
                logger.warning("Invalid login request - empty username or password");
                return Response.status(Status.BAD_REQUEST)
                        .entity(new LoginResponse("invalid request", null))
                        .build();
            }

            User user = new User(null, request.username(), request.password());
            boolean userExists = userRepo.isUserExist(user);

            if (!userExists) {
                userRepo.createUser(user);

                String token = userVetification.generateToken(request.username());

                logger.info("New user registered: " + request.username());
                return Response.ok(new LoginResponse(null, token)).build();
            } else {
                boolean passwordCorrect = userRepo.checkPassword(user);

                if (passwordCorrect) {
                    String token = userVetification.generateToken(request.username());

                    logger.info("Successful login for user: " + request.username());
                    return Response.ok(new LoginResponse(null, token)).build();

                } else {
                    logger.warning("Failed login attempt - wrong password for user: " + request.username());
                    return Response.status(Status.UNAUTHORIZED)
                            .entity(new LoginResponse("wrong password", null))
                            .build();
                }
            }
        } catch (Exception e) {
            logger.severe("Server error during login: " + e.getMessage());
            e.printStackTrace();
            return Response.status(Status.INTERNAL_SERVER_ERROR)
                    .entity(new LoginResponse("server error", null))
                    .build();
        }
    }

    /*
     * Header - token with received user's token Authorization header
     * result - close user session
     */
    @GET
    @Path("/logout")
    public Response closeSession(@HeaderParam("Authorization") String token) {
        logger.info("Logout attempt");

        try {
            // if (!RateLimiter.tryLoginConsume(httpRequest, 1)) {
            // return Response.status(Status.TOO_MANY_REQUESTS)
            // .entity(new LogoutResponse(false))
            // .build();
            // }

            if (token == null || token.trim().isEmpty()) {
                logger.warning("Logout attempt without token");
                return Response.status(Status.BAD_REQUEST)
                        .entity(new LogoutResponse(false))
                        .build();
            }

            try {
                String username = userVetification.extractUsername(token);
                logger.info("Successful logout for user: " + username);
            } catch (Exception e) {
                logger.warning("Logout attempt with invalid token: " + e.getMessage());
                return Response.status(Status.UNAUTHORIZED)
                        .entity(new LogoutResponse(false))
                        .build();
            }

            return Response.ok(new LogoutResponse(true)).build();

        } catch (Exception e) {
            logger.severe("Server error during logout: " + e.getMessage());
            e.printStackTrace();
            return Response.status(Status.INTERNAL_SERVER_ERROR)
                    .entity(new LogoutResponse(false))
                    .build();
        }
    }
}
