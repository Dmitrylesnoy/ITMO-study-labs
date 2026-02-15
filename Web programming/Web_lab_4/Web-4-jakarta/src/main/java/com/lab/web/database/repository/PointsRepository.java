package com.lab.web.database.repository;

import java.util.List;

import com.lab.web.data.PointData;

public interface PointsRepository {
    List<PointData> getAllPoints(Long userId);

    void addPoint(PointData point);
}
