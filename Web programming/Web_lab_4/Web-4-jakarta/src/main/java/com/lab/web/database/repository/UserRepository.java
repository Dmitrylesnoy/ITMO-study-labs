package com.lab.web.database.repository;

import com.lab.web.data.User;

public interface UserRepository {
    boolean isUserExist(User user);

    void createUser(User user);

    boolean checkPassword(User user);

    User getUserByUsername(String username);
}
