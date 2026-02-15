import { all } from "redux-saga/effects";
import { watcherSubmitForm } from "../features/myForm/myFormSaga";
import { watcherFetchTableData } from "../features/myTable/myTableSaga";
import { watcherLogin, watcherLogout } from "../features/auth/authSaga";

export default function* rootSaga() {
  yield all([watcherSubmitForm(), watcherFetchTableData(), watcherLogin(), watcherLogout()]);
}
