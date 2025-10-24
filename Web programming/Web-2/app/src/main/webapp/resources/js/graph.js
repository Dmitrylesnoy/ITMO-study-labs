let canvas, ctx, width, height;

function initializeCanvas() {
  canvas = document.getElementById("graph");
  if (canvas) {
    ctx = canvas.getContext("2d");
    width = 500;
    height = 500;
    window.centerX = 250;
    window.centerY = 250;
    window.scale = 40;
  }
}

function drawGraph(R) {
  if (!ctx) initializeCanvas();
  canvas.width = width;
  canvas.height = height;

  ctx.clearRect(0, 0, width, height);

  ctx.strokeStyle = "black";
  ctx.lineWidth = 2;
  ctx.font = "10px sans-serif";
  ctx.textAlign = "center";

  ctx.beginPath();
  ctx.moveTo(50, centerY);
  ctx.lineTo(450, centerY);
  ctx.moveTo(centerX, 450);
  ctx.lineTo(centerX, 50);
  ctx.stroke();

  const xTicks = [-5, -4, -3, -2, -1, 1, 2, 3, 4, 5];
  xTicks.forEach((x) => {
    const posX = centerX + x * scale;
    if (posX >= 0 && posX <= width) {
      ctx.beginPath();
      ctx.moveTo(posX, centerY - 5);
      ctx.lineTo(posX, centerY + 5);
      ctx.stroke();
      ctx.fillText(x.toString(), posX, centerY + 20);
    }
  });

  const yTicks = [-5, -4, -3, -2, -1, 1, 2, 3, 4, 5];
  ctx.textAlign = "start";
  yTicks.forEach((y) => {
    const posY = centerY - y * scale;
    if (posY >= 0 && posY <= height) {
      ctx.beginPath();
      ctx.moveTo(centerX - 5, posY);
      ctx.lineTo(centerX + 5, posY);
      ctx.stroke();
      ctx.fillText(y.toString(), centerX + 10, posY + 5);
    }
  });

  ctx.font = "12px sans-serif";
  ctx.fillText("X", 440, centerY - 10);
  ctx.fillText("Y", centerX + 10, 40);

  if (R) {
    const radiusPx = R * scale;
    ctx.fillStyle = "rgba(138,43,226,0.3)";
    ctx.strokeStyle = "#8a2be2";
    ctx.lineWidth = 2;

    // part of circle
    ctx.beginPath();
    ctx.arc(centerX, centerY, radiusPx, -Math.PI / 2, 0, false);
    ctx.lineTo(centerX, centerY);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // triangle
    ctx.beginPath();
    ctx.moveTo(centerX, centerY);
    ctx.lineTo(centerX + radiusPx / 2, centerY);
    ctx.lineTo(centerX, centerY + radiusPx / 2);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // rectangle
    const rectX = centerX - radiusPx;
    const rectY = centerY;
    const rectWidth = radiusPx;
    const rectHeight = (R / 2) * scale;
    ctx.fillRect(rectX, rectY, rectWidth, rectHeight);
    ctx.strokeRect(rectX, rectY, rectWidth, rectHeight);
  }
}

drawPoint = function (x, y, hit) {
  if (!ctx) initializeCanvas();
  const posX = centerX + x * scale;
  const posY = centerY - y * scale;

  ctx.fillStyle = hit ? "lime" : "lightcoral";
  ctx.strokeStyle = hit ? "green" : "red";
  ctx.lineWidth = 1;

  ctx.beginPath();
  ctx.arc(posX, posY, 3, 0, 2 * Math.PI);
  ctx.fill();
  ctx.stroke();
};
