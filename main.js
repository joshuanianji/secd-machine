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
      screen: {
        width: window.innerWidth,
        height: window.innerHeight,
      },
      codeExamples: codeExamples,
      initialCode: codeExamples[2]["examples"][2][1], // starts off with the lazy infinite lists
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

    // Elm gives us a page to store
    app.ports.sendPage.subscribe(([n, val]) => {
      console.log(`Page ${n} stored`);
      console.log(`val for ${n}:`, JSON.stringify(val));
      sessionStorage.setItem(toString(n), JSON.stringify(val));
    });

    // app.ports.fetchPage.subscribe((n) => {
    //   const val = JSON.parse(sessionStorage.getItem(toString(n)));
    //   app.ports.receivePage.send((n, val));
    // });
  });
});
