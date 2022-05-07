import { Elm } from "./src/Main.elm";
import CodeMirror from "codemirror";
import "codemirror/theme/material.css";
import "codemirror/lib/codemirror.css";
import "codemirror/mode/commonlisp/commonlisp";
import "codemirror/addon/edit/matchbrackets";
import "codemirror/addon/edit/closebrackets";

// loading styles after codemirror styles, so our styles override.
import "./style.scss";

import { examples as codeExamples } from "./examples/";

document.addEventListener("DOMContentLoaded", () => {
  const root = document.getElementById("app");
  const app = Elm.Main.init({
    node: root,
    flags: {
      codeExamples: codeExamples,
    },
  });

  let cm = null;

  // wait for Elm to load in #editor for us to initialize code mirror
  app.ports.initialized.subscribe((val) => {
    cm = CodeMirror(document.getElementById("editor"), {
      value: val,
      mode: "commonlisp",
      lineNumbers: true,
      lineWrapping: true,
      theme: "material",
      autofocus: true,
      autoCloseBrackets: true,
      matchBrackets: true,
    });

    cm.on("change", (_) => {
      app.ports.updatedEditor.send(cm.getValue());
    });

    app.ports.updateCode.subscribe((val) => {
      cm.setValue(val);
    });
  });
});
