import { addNewRow } from "./table";

export function loadData() {
  let keys = Object.keys(localStorage).sort();
  for (let key of keys) {
    addNewRow(JSON.parse(localStorage.getItem(key)));
  }
}

export function addData(line) {
  localStorage.setItem(localStorage.length, JSON.stringify(line));
}
