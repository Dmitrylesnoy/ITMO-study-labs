export function showLoader() {
  $("#submitBtn").prop("disabled", true);
  $('#submitBtnText').text('');
  $('#custom-bar').show();
}

export function hideLoader() {
  $("#submitBtn").prop("disabled", false);
  $('#submitBtnText').text('Check');
  $('#custom-bar').hide();
}
