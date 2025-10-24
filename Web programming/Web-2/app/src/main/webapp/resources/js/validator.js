function validateForm(data) {
  // x validating
  if (!data.x) {
    createToast("warning", "Введите значение X");
    return false;
  }
  const xStr = data.x;
  if (!/^-?\d+([.,]\d*)?$/.test(xStr)) {
    createToast("warning", "X должно быть числом, содержать только цифры, точку или запятую");
    return false;
  }
  const xNum = parseFloat(xStr);
  if (isNaN(xNum)) {
    createToast("warning", "X должно быть числом");
    return false;
  }
  if (xNum < -5 || xNum > 3) {
    createToast("warning", "X должно быть от -5 до 3");
    return false;
  }

  // y validating
  if (!data.y) {
    createToast("warning", "Введите значение Y");
    return false;
  }
  const yStr = data.y;
  if (!/^-?\d+([.,]\d*)?$/.test(yStr)) {
    createToast("warning", "Y должно быть числом, содержать только цифры, точку или запятую");
    return false;
  }
  const yNum = parseFloat(yStr);
  if (isNaN(yNum)) {
    createToast("warning", "Y должно быть числом");
    return false;
  }
  if (yNum < -5 || yNum > 3) {
    createToast("warning", "Y должно быть от -5 до 3");
    return false;
  }

  // r validating
  if (!data.r) {
    createToast("warning", "Введите значение R");
    return false;
  }
  const rStr = data.r;
  if (!/^-?\d+?$/.test(rStr)) {
    createToast("warning", "R должно быть целым числом");
    return false;
  }
  const rNum = parseFloat(rStr);
  if (isNaN(rNum)) {
    createToast("warning", "R должно быть числом");
    return false;
  }
  if (rNum < 1 || rNum > 5) {
    createToast("warning", "R должно быть целым числом от 1 до 5");
    return false;
  }

  return true;
}
