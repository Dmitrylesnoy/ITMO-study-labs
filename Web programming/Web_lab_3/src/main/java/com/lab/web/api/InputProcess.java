package com.lab.web.api;

import com.lab.web.beans.Point;
import com.lab.web.data.HitDataBean;
import com.lab.web.data.PointData;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;

@Named("inputProcess")
@RequestScoped
public class InputProcess {
    @Inject
    Point pointForm;
    @Inject
    private HitDataBean hitDataBean;

    public void submitPoint() {
        PointData pointData = new PointData();
        pointData.setX(pointForm.getX());
        pointData.setY(pointForm.getY());
        pointData.setR(pointForm.getR());

        pointData = Validator.fillPoint(pointData);
        System.out.println(pointData.toString());
        hitDataBean.addPoint(pointData);
        System.out.println(hitDataBean.toString());

        pointForm.setGraphClick(false);
    }
}
