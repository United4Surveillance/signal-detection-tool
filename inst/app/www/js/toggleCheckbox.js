function toggleCheckbox(id) {
    console.log(id);
    var checkbox = document.getElementById(id);
    var checkboxDisplay = window.getComputedStyle(checkbox, null).getPropertyValue("display");
    console.log(checkboxDisplay);
    if (checkboxDisplay == "") {
      checkbox.style.display = 'None';
    } else {
      checkbox.style.display = '';
    }
};