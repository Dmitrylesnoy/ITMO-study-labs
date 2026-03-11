"use strict";

import { addNewRow, clear, resetTable } from "./data/table";
import { loadData, addData } from "./data/storage";
import validateForm from "./data/validator";
import "../css/styles.css";
import foxImage from "../src/fox_image.jpeg";
import { showLoader, hideLoader } from "./graphic/loaderBar";
import createToast from "./graphic/toaster";

$(document).ready(function () {
  clear();
  loadData();
  document.getElementById("fox_picture").src = foxImage;
  let activeRequests = 0;

  // createToast("success","Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean aliquam nulla ut risus euismod ornare.");
  // createToast("info","Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean aliquam nulla ut risus euismod ornare.");
  // createToast("error","Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean aliquam nulla ut risus euismod ornare.");
  // createToast("warning","Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean aliquam nulla ut risus euismod ornare.");

  $("#formCoords").submit(async function (event) {
    event.preventDefault();

    const rawY = $("#y").val().trim();
    const sanitizedY = rawY.replace(/,/g, ".");
    const formData = {
      x: $('input[name="x"]:checked').val(),
      y: sanitizedY,
      r: $("#r").val(),
    };

    if (!validateForm(formData)) {
      return false;
    }

    activeRequests++;
    // $(".mdc-circular-progress").css('visibility', 'visible');
    showLoader();

    try {
      const url = "/fcgi-bin/server.jar?" + new URLSearchParams(formData);
      createToast("success", "Данные успешно отправлены!");

      const response = await fetch(url);
      // $("#formCoords")[0].reset();

      if (response.ok) {
        const data = await response.json();
        console.log("Успешный ответ:", data);

        const results = {
          x: data.x,
          y: data.y,
          r: data.r,
          hit: data.hit,
          time: new Date(data.time).toLocaleString(),
          execTime: data.execTime + " ms",
        };

        addNewRow(results);
        addData(data);
      } else {
        try {
          const errorResponse = await response.json();
          createToast(
            "error",
            "Ошибка сервера (" + response.status + "): " + errorResponse.error
          );
        } catch (e) {
          createToast(
            "error",
            "Произошла ошибка при отправке данных: " +
              response.status +
              " " +
              response.statusText
          );
        }
      }
    } catch (error) {
      createToast(
        "error",
        "Произошла ошибка при отправке данных: " + error.message
      );
      console.log(error.message);
    } finally {
      activeRequests--;
      if (activeRequests === 0) {
        // $(".mdc-circular-progress").css('visibility', 'hidden');
        hideLoader();
      }
    }
  });
});

resetTable();
