"use strict";

$(document).ready(function () {
  const rInput = document.getElementById("r");
  if (rInput && rInput.value) {
    drawGraph(rInput.value);
  } else {
    drawGraph();
  }
  if (window.points) {
    window.points.forEach((point) => {
      drawPoint(point.x, point.y, point.hit);
    });
  }

  const rBtn = document.getElementById("r-btn");

  rBtn.addEventListener("click", function () {
    drawGraph(rInput.value);
    if (window.points) {
      window.points.forEach((p) => drawPoint(p.x, p.y, p.hit));
    }
  });

  const canvas = document.getElementById("graph");
  canvas.addEventListener("click", function (event) {
    const r = rInput.value;
    if (!r) {
      createToast("warning", "Please, select R first");
      return;
    }
    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left - 20;
    const y = event.clientY - rect.top - 20;
    // console.log("x: ", x, " y: ", y);
    const graphX = ((x - centerX) / scale).toFixed(4);
    const graphY = ((centerY - y) / scale).toFixed(4);
    // console.log("CALC x: ", graphX, " y: ", graphY);

    if (validateForm({ x: graphX, y: graphY, r: r })) {
      window.location.href = "./controller?x=" + graphX + "&y=" + graphY + "&r=" + r;
    }
  });
});
