import { call, put, takeLatest, select } from "redux-saga/effects";
import { fetchTableData } from "./myTableAPI";
import { fetchTableDataRequest, fetchTableDataSuccess, fetchTableDataFailure } from "./myTableSlice";

function* fetchTableDataSaga(): Generator<any, void, any> {
  try {
    const state = yield select();
    const token = state.auth.token;

    const response = yield call(fetchTableData, token);
    if (response.data) {
      yield put(fetchTableDataSuccess(response.data));
    } else {
      yield put(fetchTableDataFailure("No data received"));
    }
  } catch (error) {
    yield put(fetchTableDataFailure(error instanceof Error ? error.message : "Unknown error"));
  }
}

export function* watcherFetchTableData() {
  yield takeLatest(fetchTableDataRequest.type, fetchTableDataSaga);
}
