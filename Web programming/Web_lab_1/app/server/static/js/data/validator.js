import createToast from "../graphic/toaster";

export default function validateForm(data) {
  if (!data.x) {
    createToast("warning","Выберите значение X");
    return false;
  }

  if (!data.y) {
    createToast("warning","Введите значение Y");
    return false;
  }

  const yStr = data.y;
  if (!/^-?\d+([.,]\d*)?$/.test(yStr)) {
    createToast("warning",
      "Y должно быть числом, содержать только цифры, точку или запятую"
    );
    return false;
  }

  const yNum = parseFloat(yStr);
  if (isNaN(yNum)) {
    createToast("warning","Y должно быть числом");
    return false;
  }
  if (yNum < -3 || yNum > 3) {
    createToast("warning","Y должно быть от -3 до 3");
    return false;
  }

  if (!data.r) {
    createToast("warning","Выберите значение R");
    return false;
  }

  const rNum = parseFloat(data.r);
  if (rNum < 0) {
    createToast("warning","R должно быть положительным числом");
    return false;
  }

  return true;
}
