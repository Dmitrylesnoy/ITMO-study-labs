package com.lab.web.databases;

import java.util.List;

import com.lab.web.data.PointData;

public interface DataAccessStrategy {
    List<PointData> getAllPoints();

    void addPoint(PointData point);
}
