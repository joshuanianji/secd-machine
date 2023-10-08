import { Elm } from "./src/Main.elm";
// use indexedDB for storage
import * as db from "idb-keyval";
import CodeMirror from "codemirror";
import "codemirror/theme/material.css";
import "codemirror/lib/codemirror.css";
import "./src/codemirror/pseudolisp";
import "codemirror/addon/edit/matchbrackets";
import "codemirror/addon/edit/closebrackets";

// loading styles after codemirror styles, so our styles override.
import "./style.scss";

import { examples as codeExamples } from "./examples/";

document.addEventListener("DOMContentLoaded", async () => {
    db.clear().then(() => {
        console.log("DB cleared");
    });
    const root = document.getElementById("app");
    const app = Elm.Main.init({
        node: root,
        flags: {
            screen: {
                width: window.innerWidth,
                height: window.innerHeight,
            },
            codeExamples: codeExamples,
            // default example to load
            defaultExample: codeExamples[0].examples[0],
        },
    });

    let cm = null;

    // wait for Elm to load in #editor for us to initialize code mirror
    app.ports.initialized.subscribe((val) => {
        cm = CodeMirror(document.getElementById("editor"), {
            value: val,
            // custom defined mode, derived from commonlisp
            mode: "pseudolisp",
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
        app.ports.sendPages.subscribe(async (pages) => {
            try {
                await db.clear();
                console.log("DB cleared");
                if (pages.length === 0) {
                    console.log("No pages to store!");
                } else {
                    console.log(`Storing pages: ${pages.map((p) => p[0])}`);
                    await db.setMany(pages);

                    console.log("Stored pages");
                }
            } catch (e) {
                console.error("Failure storing pages!");
                console.log(e);
            }
        });

        app.ports.fetchPage.subscribe((n) => {
            db.get(n)
                .then((val) => {
                    console.log(`JS: Got page ${n}`);
                    app.ports.fetchPageResponse.send([n, val]);
                })
                .catch((err) => {
                    console.log(`Failed to fetch page ${n}`, err);
                });
        });

        app.ports.log.subscribe((str) => {
            console.info(`From ELM: ${str}`);
        });

        window.onblur = () => {
            console.log("blur");
            app.ports.blurs.send(null);
        };
    });
});
