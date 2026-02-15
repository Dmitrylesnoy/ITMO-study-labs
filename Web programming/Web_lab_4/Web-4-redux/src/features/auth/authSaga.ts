import { call, put, takeLatest } from "redux-saga/effects";
import { login, logout } from "./authAPI";
import { loginRequest, loginSuccess, loginFailure, logoutRequest, logoutSuccess, logoutFailure } from "./authSlice";
import { resetForm } from "../myForm/myFormSlice";

function* loginSaga(action: ReturnType<typeof loginRequest>): Generator<any, void, any> {
  try {
    const response = yield call(login, action.payload);
    if (response.token) {
      yield put(loginSuccess({ username: action.payload.username, token: response.token }));
    } else {
      yield put(loginFailure(response.result || "Login failed"));
    }
  } catch (error) {
    yield put(loginFailure(error instanceof Error ? error.message : "Unknown error"));
  }
}

function* logoutSaga(action: ReturnType<typeof logoutRequest>): Generator<any, void, any> {
  try {
    const response = yield call(logout, action.payload);
    if (response.success) {
      yield put(logoutSuccess());
      yield put(resetForm());
    } else {
      yield put(logoutFailure(response.error || "Logout failed"));
    }
  } catch (error) {
    yield put(logoutFailure(error instanceof Error ? error.message : "Unknown error"));
  }
}

export function* watcherLogin() {
  yield takeLatest(loginRequest.type, loginSaga);
}

export function* watcherLogout() {
  yield takeLatest(logoutRequest.type, logoutSaga);
}
