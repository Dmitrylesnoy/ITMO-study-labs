package com.lab.web.databases;

import java.util.List;

import com.lab.web.data.PointData;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.transaction.Transactional;

@Transactional
public class HibernateDataAccessStrategy implements DataAccessStrategy {
    @PersistenceContext(unitName = "com.lab.web3")
    private EntityManager entityManager;

    @Override
    public List<PointData> getAllPoints() {
        TypedQuery<PointData> query = entityManager.createQuery("SELECT p FROM PointData p ORDER BY p.date DESC",
                PointData.class);
        return query.getResultList();
    }

    @Override
    public void addPoint(PointData point) {
        System.out.println("Hibernate: point added");
        entityManager.persist(point);
    }
}
