package com.lab.web.utils.auth;

import java.util.logging.Logger;

import com.lab.web.data.User;
import com.lab.web.database.repository.UserRepository;

import io.jsonwebtoken.Claims;
import jakarta.ejb.Stateless;
import jakarta.inject.Inject;
import jakarta.security.auth.message.AuthException;

@Stateless
public class UserVetification {
    private static final Logger logger = Logger.getLogger(UserVetification.class.getName());

    @Inject
    private UserRepository dataAccess;

    private static JwtService jwtService = new JwtService();

    public String generateToken(String username) {
        return jwtService.generateToken(username);
    }

    private String normalizeToken(String token) throws AuthException {
        if (token == null || token.trim().isEmpty()) {
            logger.warning("Fetch attempt without token");
            throw new AuthException("Token required");
        }
        if (token.startsWith("Bearer ")) {
            return token.substring(7);
        }
        return token;
    }

    public String extractUsername(String token) throws AuthException {
        token = normalizeToken(token);
        return jwtService.extractUsername(token);
    }

    public void checkUserByToken(String token) throws AuthException {
        token = normalizeToken(token);
        try {
            jwtService.extractClaims(token);
        } catch (Exception e) {
            logger.warning("Fetch attempt with invalid token: " + e.getMessage());
            throw new AuthException("Invalid token");
        }
    }

    public Long getUserIDbyToken(String token) throws AuthException {
        token = normalizeToken(token);
        try {
            Claims claims = jwtService.extractClaims(token);
            String username = claims.getSubject();

            User user = dataAccess.getUserByUsername(username);
            if (user == null) {
                throw new AuthException("User not found");
            }

            return user.id();
        } catch (Exception e) {
            logger.warning("Invalid token: " + e.getMessage());
            throw new AuthException("Invalid token");
        }
    }
}
