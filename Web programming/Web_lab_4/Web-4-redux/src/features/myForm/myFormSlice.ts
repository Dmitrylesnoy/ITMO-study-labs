import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { RootState } from "../../app/store";
import { FormData, ServerResponse } from "./myFormAPI";

export interface MyFormState {
  x: number | null;
  y: number | null;
  r: number | null;
  xError: string;
  yError: string;
  rError: string;
  formError: string;
}

const initialState: MyFormState = {
  x: null,
  y: null,
  r: null,
  xError: "",
  yError: "",
  rError: "",
  formError: "",
};

export const myFormSlice = createSlice({
  name: "myForm",
  initialState,
  reducers: {
    setX: (state, action: PayloadAction<number | null>) => {
      state.x = action.payload;
      state.xError = "";
    },
    setY: (state, action: PayloadAction<number | null>) => {
      state.y = action.payload;
      state.yError = "";
    },
    setR: (state, action: PayloadAction<number | null>) => {
      state.r = action.payload;
      state.rError = "";
    },
    validateX: (state) => {
      if (state.x === null) {
        state.xError = "X is required";
      } else if (state.x < -3 || state.x > 5) {
        state.xError = "X must be between -3 and 5";
      } else {
        state.xError = "";
      }
    },
    validateY: (state) => {
      if (state.y === null) {
        state.yError = "Y is required, must be a number";
      } else if (state.y < -5 || state.y > 5) {
        state.yError = "Y must be between -5 and 5";
      } else {
        state.yError = "";
      }
    },
    validateR: (state) => {
      if (state.r === null) {
        state.rError = "R is required";
      } else if (state.r < 0 || state.r > 5) {
        state.rError = "R must be between 0 and 5";
      } else {
        state.rError = "";
      }
    },
    resetForm: (state) => {
      return { ...initialState };
    },
    submitFormRequest: (state, action: PayloadAction<FormData>) => {
      state.formError = "";
    },
    submitFormSuccess: (state, action: PayloadAction<ServerResponse>) => {
      state.formError = "";
    },
    submitFormFailure: (state, action: PayloadAction<string>) => {
      state.formError = action.payload;
    },
  },
});

export const {
  setX,
  setY,
  setR,
  validateX,
  validateY,
  validateR,
  resetForm,
  submitFormRequest,
  submitFormSuccess,
  submitFormFailure,
} = myFormSlice.actions;

export const selectMyForm = (state: RootState) => state.myForm;

export default myFormSlice.reducer;
