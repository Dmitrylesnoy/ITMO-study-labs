import createToast from "../graphic/toaster";
import { add } from "./storage";

export function addNewRow(data) {
  let rowClass = "";
  if (data.hit === "Yes") {
    rowClass = "hit-yes";
  } else if (data.hit === "No") {
    rowClass = "hit-no";
  }
  $("#output").prepend(`
                    <tr class="${rowClass}">
                      <td>${data.x}</td>
                      <td>${data.y}</td>
                      <td>${data.r}</td>
                      <td>${data.hit}</td>
                      <td>${data.execTime}</td>
                      <td>${data.time}</td>
                    </tr>
                `);
}

export function resetTable() {
  $("#resetBtn").click(function (event) {
    event.preventDefault();
    localStorage.clear();
    clear();
    createToast("info","Таблица очищена");
  });
}

export function clear(){
    $("#output").empty();
}