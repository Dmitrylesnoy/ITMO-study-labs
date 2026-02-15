"use strict";

$(document).ready(function () {
  initializeCanvas();
  updateGraph();

  $(document).on("click", String.raw`#formCoords\:resetBtn`, function () {
    setTimeout(() => {
      updateGraph();
    }, 100);
  });
  $(document).on("click", String.raw`#formCoords\:r-button`, function () {
    setTimeout(() => {
      updateGraph();
    }, 100);
  });

  $(document).on("click", String.raw`#formCoords\:submitBtn`, function () {
    setTimeout(() => {
      updateGraph();
    }, 100);
  });

  const canvas = document.getElementById("graph");
  if (canvas) {
    canvas.addEventListener("click", function (event) {
      const rInput = document.getElementById("formCoords:r-button");
      const r = rInput ? parseFloat(rInput.value) : null;
      if (!r || r === 0) {
        alert("Please, select R first (R cannot be zero)");
        return;
      }
      const rect = canvas.getBoundingClientRect();
      const x = event.clientX - rect.left - 20;
      const y = event.clientY - rect.top - 20;
      const graphX = ((x - centerX) / scale).toFixed(4);
      const graphY = ((centerY - y) / scale).toFixed(4);

      updateXHidden(graphX);

      const radios = document.querySelectorAll('input[name="formCoords:xInput_x"]');
      radios.forEach((radio) => (radio.checked = false));

      const yInput = document.getElementById("formCoords:y-text");
      yInput.value = graphY;

      const graphClickFlag = document.getElementById("formCoords:graphClickFlag");
      console.log(graphClickFlag.value);
      graphClickFlag.value = "true";
      console.log(graphClickFlag.value);

      const submitBtn = document.getElementById("formCoords:submitBtn");
      submitBtn.click();
    });
  }
});

function updateXHidden(value) {
  const hiddenInput = document.getElementById("formCoords:xInput_xHidden");
  hiddenInput.value = value;
}
