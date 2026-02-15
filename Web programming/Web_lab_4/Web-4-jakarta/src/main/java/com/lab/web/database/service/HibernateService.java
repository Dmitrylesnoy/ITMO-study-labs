package com.lab.web.database.service;

import java.util.List;
import java.util.logging.Logger;

import com.lab.web.data.PointData;
import com.lab.web.data.User;
import com.lab.web.database.repository.PointsRepository;
import com.lab.web.database.repository.UserRepository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.NoResultException;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.TypedQuery;
import jakarta.transaction.Transactional;

@Transactional
public class HibernateService implements UserRepository, PointsRepository {
    @PersistenceContext(unitName = "com.lab.web3")
    private EntityManager entityManager;

    private static final Logger logger = Logger.getLogger(HibernateService.class.getName());

    @Override
    public List<PointData> getAllPoints(Long userId) {
        TypedQuery<PointData> query = entityManager.createQuery(
                "SELECT p FROM PointData p WHERE p.user_id=:userId ORDER BY p.date DESC",
                PointData.class);
        query.setParameter("userId", userId);
        return query.getResultList();
    }

    @Override
    public void addPoint(PointData point) {
        logger.info("Hibernate: point added");
        entityManager.persist(point);
    }

    @Override
    public boolean isUserExist(User user) {
        Query query = entityManager.createQuery("SELECT COUNT(u) FROM User u WHERE u.username = :username");
        query.setParameter("username", user.username());
        Long count = (Long) query.getSingleResult();
        return count > 0;
    }

    @Override
    public void createUser(User user) {
        entityManager.persist(user);
        logger.info("Hibernate: user created - " + user.username());
    }

    @Override
    public boolean checkPassword(User user) {
        Query query = entityManager
                .createQuery("SELECT u FROM User u WHERE u.username = :username AND u.password = :password");
        query.setParameter("username", user.username());
        query.setParameter("password", user.password());
        try {
            User foundUser = (User) query.getSingleResult();
            return foundUser != null;
        } catch (NoResultException e) {
            return false;
        }
    }

    @Override
    public User getUserByUsername(String username) {
        Query query = entityManager.createQuery("SELECT u FROM User u WHERE u.username = :username");
        query.setParameter("username", username);
        try {
            return (User) query.getSingleResult();
        } catch (NoResultException e) {
            return null;
        }
    }
}
