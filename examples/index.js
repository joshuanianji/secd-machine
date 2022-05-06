// importing all examples

// BASICS
import { plainText as arithmetic } from "./basics/arithmetic.lisp";
import { plainText as comparison } from "./basics/comparison.lisp";
import { plainText as ifStmt } from "./basics/if.lisp";
import { plainText as letStmt } from "./basics/let.lisp";
import { plainText as letrec } from "./basics/letrec.lisp";

// Lists
import { plainText as listLength } from "./lists/listLength.lisp";
import { plainText as mapList } from "./lists/mapList.lisp";
import { plainText as sumList } from "./lists/sumList.lisp";

// complex
import { plainText as currying } from "./complex/currying.lisp";
import { plainText as infiniteList } from "./complex/infiniteList.lisp";
import { plainText as mutualrec } from "./complex/mutualrec.lisp";

export const examples = [
  {
    type: "basics",
    examples: [arithmetic, comparison, ifStmt, letStmt, letrec],
  },
  {
    type: "lists",
    examples: [listLength, mapList, sumList],
  },
  {
    type: "complex",
    examples: [currying, infiniteList, mutualrec],
  },
];
