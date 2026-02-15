package com.lab.web.data;

import java.io.Serializable;
import java.util.List;

import com.lab.web.database.repository.PointsRepository;
import jakarta.inject.Inject;

import jakarta.ejb.Singleton;

@Singleton
public class HitDataBean implements Serializable {
    @Inject
    private PointsRepository pointsRepo;

    public List<PointData> getData(Long userId) {
        return pointsRepo.getAllPoints(userId);
    }

    public void addPoint(PointData point) {
        pointsRepo.addPoint(point);
    }
}
