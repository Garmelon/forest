"use strict";

const settingsDiv = document.getElementById("settings");
const settingsButton = settingsDiv.querySelector("button");
const settingsForm = settingsDiv.querySelector("form");
let settingsMenuState;
settingsButton.addEventListener("click", event => setSettingsMenuState(!settingsMenuState));
window.addEventListener("load", event => setSettingsMenuState(false));

function setSettingsMenuState(open) {
    settingsMenuState = open;
    if (open) {
        settingsDiv.style.transform = "none";
    } else {
        let height = settingsButton.offsetHeight;
        settingsDiv.style.transform = `translateY(calc(100% - ${height}px))`;
    }
}

const curvyLinesCheckbox = document.getElementById("curvy-lines-checkbox");
curvyLinesCheckbox.addEventListener("change", event => setCurvyLines(event.target.checked));
window.addEventListener("load", event => {
    let curvy = window.localStorage.getItem("curvy");
    curvyLinesCheckbox.checked = curvy;
    setCurvyLines(curvy);
});

function setCurvyLines(curvy) {
    document.body.classList.toggle("curvy", curvy);
    if (curvy) {
        window.localStorage.setItem("curvy", "yes");
    } else {
        window.localStorage.removeItem("curvy");
    }
}
