let acutisPkg = require("../../package.json");

module.exports = {
  title: "Acutis",
  subtitle: acutisPkg.description,
  url:
    process.env.NODE_ENV === "production"
      ? "https://johnridesa.bike/acutis"
      : "http://localhost:8080",
  menu: [
    { path: "/", name: "Home" },
    { path: "/introduction/", name: "Getting started" },
    { path: "/manual/", name: "Language manual" },
    { path: "/api/", name: "API" },
    { path: "/playground/", name: "Playground" },
    { path: "/development/", name: "Development" },
    { path: "/credits/", name: "Credits" },
  ],
};
