import React, { useRef, useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { RootState } from "../../app/store";
import { selectMyForm, submitFormRequest } from "../myForm/myFormSlice";
import { useAppDispatch } from "../../app/hooks";
import { PointData } from "./../myTable/myTableSlice";

const BASE_SIZE = 500;
const MOBILE_SIZE = 350;

export function drawPoint(
  canvas: HTMLCanvasElement | null,
  x: number,
  y: number,
  hit: boolean,
  scale: number,
  centerX: number,
  centerY: number
) {
  if (!canvas) return;

  const ctx = canvas.getContext("2d");
  if (!ctx) return;

  const posX = centerX + x * scale;
  const posY = centerY - y * scale;

  ctx.fillStyle = hit ? "lime" : "lightcoral";
  ctx.strokeStyle = hit ? "green" : "red";
  ctx.lineWidth = 1;

  ctx.beginPath();
  ctx.arc(posX, posY, 3, 0, 2 * Math.PI);
  ctx.fill();
  ctx.stroke();
}

export function Graph() {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const { r } = useSelector(selectMyForm);
  const tableData = useSelector((state: RootState) => state.myTable.data);
  const dispatch = useAppDispatch();
  const [canvasSize, setCanvasSize] = useState(BASE_SIZE);

  useEffect(() => {
    const handleResize = () => {
      if (window.innerWidth <= 886) {
        setCanvasSize(MOBILE_SIZE);
      } else if (window.innerWidth <= 1191) {
        setCanvasSize(MOBILE_SIZE);
      } else {
        setCanvasSize(BASE_SIZE);
      }
    };

    handleResize();
    window.addEventListener("resize", handleResize);
    return () => window.removeEventListener("resize", handleResize);
  }, []);

  const CENTER_X = canvasSize / 2;
  const CENTER_Y = canvasSize / 2;
  const SCALE = canvasSize / 12;

  const drawGraph = (R: number | null) => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    const width = canvasSize;
    const height = canvasSize;

    canvas.width = width;
    canvas.height = height;

    ctx.clearRect(0, 0, width, height);

    ctx.strokeStyle = "black";
    ctx.lineWidth = Math.max(1, (canvasSize / 500) * 2);
    ctx.font = `${Math.max(8, (canvasSize / 500) * 10)}px sans-serif`;
    ctx.textAlign = "center";

    // Draw axes
    ctx.beginPath();
    ctx.moveTo(width * 0.1, CENTER_Y);
    ctx.lineTo(width * 0.9, CENTER_Y);
    ctx.moveTo(CENTER_X, height * 0.9);
    ctx.lineTo(CENTER_X, height * 0.1);
    ctx.stroke();

    // Draw X ticks
    const xTicks = [-5, -4, -3, -2, -1, 1, 2, 3, 4, 5];
    xTicks.forEach((x) => {
      const posX = CENTER_X + x * SCALE;
      if (posX >= width * 0.1 && posX <= width * 0.9) {
        ctx.beginPath();
        ctx.moveTo(posX, CENTER_Y - 5);
        ctx.lineTo(posX, CENTER_Y + 5);
        ctx.stroke();
        ctx.fillText(x.toString(), posX, CENTER_Y + Math.max(15, (canvasSize / 500) * 20));
      }
    });

    // Draw Y ticks
    const yTicks = [-5, -4, -3, -2, -1, 1, 2, 3, 4, 5];
    ctx.textAlign = "start";
    yTicks.forEach((y) => {
      const posY = CENTER_Y - y * SCALE;
      if (posY >= height * 0.1 && posY <= height * 0.9) {
        ctx.beginPath();
        ctx.moveTo(CENTER_X - 5, posY);
        ctx.lineTo(CENTER_X + 5, posY);
        ctx.stroke();
        ctx.fillText(
          y.toString(),
          CENTER_X + Math.max(8, (canvasSize / 500) * 10),
          posY + Math.max(3, (canvasSize / 500) * 5)
        );
      }
    });

    // Draw axis labels
    ctx.font = `${Math.max(10, (canvasSize / 500) * 12)}px sans-serif`;
    ctx.fillText("X", width * 0.88, CENTER_Y - Math.max(8, (canvasSize / 500) * 10));
    ctx.fillText("Y", CENTER_X + Math.max(8, (canvasSize / 500) * 10), height * 0.12);

    if (R && R > 0) {
      const radiusPx = R * SCALE;
      ctx.fillStyle = "rgba(138,43,226,0.3)";
      ctx.strokeStyle = "#8a2be2";
      ctx.lineWidth = Math.max(1, (canvasSize / 500) * 2);

      // Draw circle sector
      ctx.beginPath();
      ctx.arc(CENTER_X, CENTER_Y, radiusPx, -Math.PI, -Math.PI / 2, false);
      ctx.lineTo(CENTER_X, CENTER_Y);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      // Draw triangle
      ctx.beginPath();
      ctx.moveTo(CENTER_X, CENTER_Y);
      ctx.lineTo(CENTER_X - radiusPx, CENTER_Y);
      ctx.lineTo(CENTER_X, CENTER_Y + radiusPx / 2);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      // Draw rectangle
      const rectX = CENTER_X;
      const rectY = CENTER_Y;
      const rectWidth = radiusPx;
      const rectHeight = radiusPx;
      ctx.fillRect(rectX, rectY, rectWidth, rectHeight);
      ctx.strokeRect(rectX, rectY, rectWidth, rectHeight);
    }
  };

  useEffect(() => {
    const canvas = canvasRef.current;
    if (canvas) {
      drawGraph(r);
      if (tableData == null) {
        console.log("table data is null/undefined");
      } else {
        tableData.forEach((point: PointData) => {
          drawPoint(canvas, point.x, point.y, point.hit, SCALE, CENTER_X, CENTER_Y);
        });
      }
    } else console.log("Graph counld not be init");
  }, [tableData, r, SCALE, CENTER_X, CENTER_Y]);

  const handleCanvasClick = (event: React.MouseEvent<HTMLCanvasElement>) => {
    if (!r || r <= 0) {
      alert("Please select R first (R cannot be zero)");
      return;
    }

    const canvas = canvasRef.current;
    if (!canvas) return;

    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;

    const graphX = ((x - CENTER_X) / SCALE).toFixed(4);
    const graphY = ((CENTER_Y - y) / SCALE).toFixed(4);

    const graphData = {
      x: +graphX,
      y: +graphY,
      r,
      graphFlag: true,
    };

    console.log(`Clicked at X: ${graphX}, Y: ${graphY}`);
    dispatch(submitFormRequest(graphData));
  };

  return (
    <div className="card" style={{ display: "flex", justifyContent: "center", padding: "20px" }}>
      <canvas
        ref={canvasRef}
        width={canvasSize}
        height={canvasSize}
        onClick={handleCanvasClick}
        style={{
          maxWidth: "100%",
          height: "auto",
          cursor: r && r > 0 ? "crosshair" : "not-allowed",
          border: "1px solid #ccc",
          borderRadius: "8px",
        }}
      />
    </div>
  );
}
