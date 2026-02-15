package com.lab.web.utils.auth;

import at.favre.lib.crypto.bcrypt.BCrypt;

public class PasswordService {
    private PasswordService() {
    }

    public static String hashPassword(String password) {
        return BCrypt.withDefaults().hashToString(12, password.toCharArray());
    }

    public static boolean verifyPassword(String password, String hashedPassword) {
        BCrypt.Result result = BCrypt.verifyer().verify(password.toCharArray(), hashedPassword);
        return result.verified;
    }
}
