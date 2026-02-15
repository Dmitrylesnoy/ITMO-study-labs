import React, { useEffect } from "react";
import { useSelector } from "react-redux";
import { RootState } from "../../app/store";
import { fetchTableDataRequest, PointData } from "./myTableSlice";
import styles from "./MyTable.module.css";
import { useAppDispatch } from "../../app/hooks";

export function MyTable() {
  const tableData = useSelector((state: RootState) => state.myTable.data);
  const dispatch = useAppDispatch();

  const auth = useSelector((state: RootState) => state.auth.isAuthenticated);

  useEffect(() => {
    if (auth) dispatch(fetchTableDataRequest());
  }, [dispatch, auth]);

  if (!tableData) {
    return <div>Loading data...</div>;
  }
  return (
    <div className={styles.table}>
      <div className={styles.responsiveTable}>
        <table className={styles.tableData}>
          <thead>
            <tr>
              <th className={styles.hideMobile}>X</th>
              <th className={styles.hideMobile}>Y</th>
              <th className={styles.hideMobile}>R</th>
              <th>Hit</th>
              <th className={styles.hideMobile}>Calc time</th>
              <th className={styles.hideMobile}>Current time</th>
              <th className={styles.showMobileOnly}>Point</th>
            </tr>
          </thead>
          <tbody>
            {tableData.map((pointData: PointData, index: number) => (
              <tr key={index} className={pointData.hit ? styles.hitYes : styles.hitNo}>
                <td className={styles.hideMobile}>{pointData.x}</td>
                <td className={styles.hideMobile}>{pointData.y}</td>
                <td className={styles.hideMobile}>{pointData.r}</td>
                <td>{pointData.hit ? "Yes" : "No"}</td>
                <td className={styles.hideMobile}>{pointData.execTime} ns</td>
                <td className={styles.hideMobile}>{pointData.dateFormatted}</td>
                <td className={styles.showMobileOnly}>
                  ({pointData.x}, {pointData.y}) R:{pointData.r}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
