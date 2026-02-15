import { createSlice, PayloadAction } from "@reduxjs/toolkit";

export interface PointData {
  x: number;
  y: number;
  r: number;
  hit: boolean;
  execTime: number;
  dateFormatted: string;
}

export interface MyTableState {
  data?: PointData[];
  error?: string;
}

const initialState: MyTableState = {
  data: undefined,
};

const myTableSlice = createSlice({
  name: "myTable",
  initialState,
  reducers: {
    setTableData: (state, action: PayloadAction<PointData[]>) => {
      state.data = action.payload;
    },
    addTableData: (state, action: PayloadAction<PointData>) => {
      if (state.data) {
        state.data.unshift(action.payload);
      } else {
        state.data = [action.payload];
      }
    },
    clearTableData: (state) => {
      state.data = [];
    },
    fetchTableDataRequest: (state) => {
      state.error = undefined;
    },
    fetchTableDataSuccess: (state, action: PayloadAction<PointData[]>) => {
      state.data = action.payload;
    },
    fetchTableDataFailure: (state, action: PayloadAction<string>) => {
      state.error = action.payload;
    },
  },
});

export const {
  setTableData,
  addTableData,
  clearTableData,
  fetchTableDataRequest,
  fetchTableDataSuccess,
  fetchTableDataFailure,
} = myTableSlice.actions;

export default myTableSlice.reducer;
