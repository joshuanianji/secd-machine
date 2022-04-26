import "./style.scss";
import { Elm } from "./src/Main.elm";
import CodeMirror from "codemirror";
import "codemirror/theme/material.css";
import "codemirror/lib/codemirror.css";
import "codemirror/mode/commonlisp/commonlisp";
import "codemirror/addon/edit/matchbrackets";
import "codemirror/addon/edit/closebrackets";

document.addEventListener("DOMContentLoaded", () => {
  const root = document.getElementById("app");
  const app = Elm.Main.init({ node: root });

  const cm = CodeMirror(document.getElementById("editor"), {
    value: "",
    mode: "commonlisp",
    lineNumbers: true,
    lineWrapping: true,
    theme: "material",
    autofocus: true,
    autoCloseBrackets: true,
    matchBrackets: true,
  });

  cm.on("change", (instance, changeObj) => {
    app.ports.updatedEditor.send(cm.getValue());
  });
});
