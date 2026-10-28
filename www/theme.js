(function () {
  "use strict";

  var themes = ["dark", "light", "high-contrast"];
  var labels = {
    dark: "&#9790; Dark",
    light: "&#9728; Light",
    "high-contrast": "&#9681; High Contrast"
  };

  function applyTheme(theme) {
    var validTheme = themes.indexOf(theme) >= 0 ? theme : "dark";
    document.body.classList.remove("dark-theme", "light-theme", "high-contrast-theme");
    document.body.classList.add(validTheme + "-theme");

    var button = document.getElementById("theme-toggle-btn");
    if (button) {
      button.innerHTML = labels[validTheme];
      button.setAttribute("aria-label", "Current theme: " + validTheme +
        ". Click to change theme.");
      button.title = "Current theme: " + validTheme + " (click to change)";
    }

    try {
      window.localStorage.setItem("openaq-theme", validTheme);
    } catch (error) {
      // The selected theme still applies for this page if storage is unavailable.
    }
  }

  window.toggleTheme = function () {
    var current = themes.find(function (theme) {
      return document.body.classList.contains(theme + "-theme");
    }) || "dark";
    applyTheme(themes[(themes.indexOf(current) + 1) % themes.length]);
  };

  document.addEventListener("DOMContentLoaded", function () {
    var savedTheme = "dark";
    try {
      savedTheme = window.localStorage.getItem("openaq-theme") || "dark";
    } catch (error) {
      // Use the default theme if browser storage is unavailable.
    }
    applyTheme(savedTheme);

    if (window.Shiny) {
      window.Shiny.addCustomMessageHandler("clearSessionFiles", function () {
        ["upload_csv", "batch_files_stack", "batch_files_merge"].forEach(function (id) {
          var input = document.getElementById(id);
          if (input) {
            input.value = "";
            input.dispatchEvent(new Event("change", { bubbles: true }));
          }
        });
      });
    }
  });
}());
