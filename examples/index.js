// importing all examples

// BASICS
import { plainText as arithmetic } from "./basics/arithmetic.lisp";
import { plainText as comparison } from "./basics/comparison.lisp";
import { plainText as ifStmt } from "./basics/if.lisp";
import { plainText as letStmt } from "./basics/let.lisp";
import { plainText as letstar } from "./basics/letstar.lisp";

// Lists
import { plainText as listLength } from "./lists/listLength.lisp";
import { plainText as mapList } from "./lists/mapList.lisp";
import { plainText as sumList } from "./lists/sumList.lisp";

// complex
import { plainText as currying } from "./complex/currying.lisp";
import { plainText as infiniteList } from "./complex/infiniteList.lisp";
import { plainText as mutualrec } from "./complex/mutualrec.lisp";
import { plainText as shadowing } from "./complex/shadowing.lisp";

export const examples = [
  {
    type: "basics",
    examples: [
      ["Arithmetic", arithmetic],
      ["Value Comparison", comparison],
      ["If Statement", ifStmt],
      ["Let Statements", letStmt],
      ["Simulating Let*", letstar],
    ],
  },
  {
    type: "lists",
    examples: [
      ["Length of a List - Recursion", listLength],
      ["List Map", mapList],
      ["Summing a List", sumList],
    ],
  },
  {
    type: "complex",
    examples: [
      ["Currying", currying],
      ["Shadowing", shadowing],
      ["Lazy Infinite Lists", infiniteList],
      ["Mutual Recursion", mutualrec],
    ],
  },
];
