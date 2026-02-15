import React, { ChangeEvent, FormEvent } from "react";
import styles from "./MyForm.module.css";
import {
  setX,
  setY,
  setR,
  validateX,
  validateY,
  validateR,
  resetForm,
  selectMyForm,
  submitFormRequest,
} from "./myFormSlice";
import { useAppDispatch, useAppSelector } from "../../app/hooks";

export function MyForm() {
  const dispatch = useAppDispatch();
  const { x, y, r, xError, yError, rError, formError } = useAppSelector(selectMyForm);

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    dispatch(validateX());
    dispatch(validateY());
    dispatch(validateR());
    if (!xError && !yError && !rError && x !== null && y !== null && r !== null) {
      dispatch(submitFormRequest({ x, y, r, graphFlag: false }));
    }
  };

  const handleX = (e: ChangeEvent<HTMLInputElement>) => {
    dispatch(setX(+e.target.value));
    dispatch(validateX());
  };
  const handleY = (e: ChangeEvent<HTMLInputElement>) => {
    const val = e.target.value === "" ? null : +e.target.value;
    dispatch(setY(val));
    dispatch(validateY());
  };
  const handleR = (e: ChangeEvent<HTMLInputElement>) => {
    dispatch(setR(+e.target.value));
    dispatch(validateR());
  };

  return (
    <div className={styles.formContainer}>
      <form id="formCoords" className={styles.formCoords} onSubmit={handleSubmit}>
        <div className={styles.coordsGroup}>
          <div className={styles.inputGroup}>
            <label htmlFor="x" className={styles.requiredField}>
              X:
            </label>
            <div className={styles.radio}>
              {[-3, -2, -1, 0, 1, 2, 3, 4, 5].map((val) => (
                <label key={val}>
                  <input type="radio" name="x" value={val} checked={x === val} onChange={handleX} />
                  {val}
                </label>
              ))}
            </div>
            {xError && <div className={styles.errorMessage}>{xError}</div>}
          </div>
          <div className={styles.inputGroup}>
            <label htmlFor="y" className={styles.requiredField}>
              Y:
            </label>
            <input
              id="y"
              type="number"
              value={y ?? ""}
              onChange={handleY}
              maxLength={8}
              placeholder="От -5 до 5"
              required
            />
            <br />
            {yError && <div className={styles.errorMessage}>{yError}</div>}
          </div>
          <div id="r-group" className={styles.inputGroup}>
            <label htmlFor="r" className={styles.requiredField}>
              R:
            </label>
            <div className={styles.radio}>
              {[-3, -2, -1, 0, 1, 2, 3, 4, 5].map((val) => (
                <label key={val}>
                  <input type="radio" name="r" value={val} checked={r === val} onChange={handleR} />
                  {val}
                </label>
              ))}
            </div>
            {rError && <div className={styles.errorMessage}>{rError}</div>}
          </div>
        </div>
        <div className={styles.buttonGroup}>
          <button type="submit">Check</button>
          <button type="button" onClick={() => dispatch(resetForm())}>
            Reset
          </button>
        </div>
        {formError && <div className={styles.errorMessage}>{formError}</div>}
      </form>
    </div>
  );
}
