(function () {
  "use strict";

  var themes = ["dark", "light"];
  var labels = {
    dark: "&#9790; Dark",
    light: "&#9728; Light"
  };

  function applyTheme(theme) {
    var selected = themes.indexOf(theme) >= 0 ? theme : "dark";
    document.body.classList.remove(
      "dark-theme", "light-theme"
    );
    document.body.classList.add(selected + "-theme");

    var button = document.getElementById("theme-toggle-btn");
    if (button) {
      button.innerHTML = labels[selected];
      button.setAttribute("aria-label", "Current theme: " + selected +
        ". Click to change theme.");
      button.title = "Current theme: " + selected + " (click to change)";
    }

    try {
      window.localStorage.setItem("openaq-theme", selected);
    } catch (error) {
      // Apply the theme for this page even when browser storage is unavailable.
    }
  }

  window.toggleTheme = function () {
    var current = themes.find(function (theme) {
      return document.body.classList.contains(theme + "-theme");
    }) || "dark";
    applyTheme(themes[(themes.indexOf(current) + 1) % themes.length]);
  };

  function initialize() {
    var selected = "dark";
    try {
      selected = window.localStorage.getItem("openaq-theme") || "dark";
    } catch (error) {
      // Keep the default theme if browser storage is unavailable.
    }
    applyTheme(selected);
    document.body.classList.add("sidebar-mini");
    document.body.classList.remove("sidebar-collapse");

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
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
}());
