function createToast(type, text) {
  let notifications = document.querySelector(".notifications");
  if (!notifications) {
    console.error("Notifications container not found");
    return;
  }

  let icon, title;

  switch (type) {
    case "info":
      icon = "fa fa-info-circle";
      title = "Information";
      break;
    case "success":
      icon = "fa fa-check-circle";
      title = "Success";
      break;
    case "warning":
      icon = "fa fa-exclamation-triangle";
      title = "Warning";
      break;
    case "error":
      icon = "fa fa-exclamation-circle";
      title = "Error";
      break;
  }
  let newToast = document.createElement("div");
  newToast.innerHTML = `<div class="toast ${type}">
                            <div class="icon">
                                <i class="${icon}"></i>
                            </div>
                            <div class="toastText">
                                <h1>${title}</h1>
                                <p>${text}</p>
                            </div>
                            <i class="fa fa-times close-btn" onclick="this.parentElement.remove()"></i>
                        </div>`;
  notifications.prepend(newToast);
  newToast.timeoutId = setTimeout(() => newToast.remove(), 5000);
}
