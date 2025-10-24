package web.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;

public class HitData implements Serializable{

    private ArrayList<Point> dataList = new ArrayList<>();

    public HitData() {
    }

    public ArrayList<Point> getDataList() {
        return dataList;
    }

    public void addPoint(Point point) {
        dataList.add(point);
    }

    public void reset() {
        dataList.clear();
    }

    public ArrayList<Point> getReversedDataList() {
        ArrayList<Point> reversed = new ArrayList<>(dataList);
        Collections.reverse(reversed);
        return reversed;
    }

}
