package com.lab.web.data;

import java.io.Serializable;
import java.util.List;

import com.lab.web.databases.DataAccessStrategy;
import com.lab.web.databases.JDBCDataAccessStrategy;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;
import jakarta.transaction.Transactional;

@Named("hitData")
@Transactional
@ApplicationScoped
public class HitDataBean implements Serializable {
    private DataAccessStrategy dataAccessStrategy;

    public HitDataBean() {
        this.dataAccessStrategy = new JDBCDataAccessStrategy();
    }

    public List<PointData> getData() {
        return dataAccessStrategy.getAllPoints();
    }

    public void addPoint(PointData point) {
        dataAccessStrategy.addPoint(point);
    }

    public String getDataAsJson() {
        List<PointData> data = getData();
        if (data.isEmpty()) {
            return "[]";
        }

        StringBuilder json = new StringBuilder();
        json.append("[");

        for (int i = 0; i < data.size(); i++) {
            PointData point = data.get(i);
            json.append("{");
            json.append("\"x\":").append(point.getX()).append(",");
            json.append("\"y\":").append(point.getY()).append(",");
            json.append("\"r\":").append(point.getR()).append(",");
            json.append("\"hit\":").append(point.isHit());
            json.append("}");

            if (i < data.size() - 1) {
                json.append(",");
            }
        }
        json.append("]");
        return json.toString();
    }
}
