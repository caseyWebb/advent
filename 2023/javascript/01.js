"use strict";

const numbers = {
  zero: 0,
  0: 0,
  one: 1,
  1: 1,
  two: 2,
  2: 2,
  three: 3,
  3: 3,
  four: 4,
  4: 4,
  five: 5,
  5: 5,
  six: 6,
  6: 6,
  seven: 7,
  7: 7,
  eight: 8,
  8: 8,
  nine: 9,
  9: 9,
};

function getFirstNum(str) {
  let i = Infinity;
  let n;
  for (const k of Object.keys(numbers)) {
    const i_ = str.indexOf(k);
    if (i_ > -1 && i_ < i) {
      i = i_;
      n = numbers[k];
    }
  }
  return n;
}

function getLastNum(str) {
  let i = -1;
  let n;
  for (const k of Object.keys(numbers)) {
    const i_ = str.lastIndexOf(k);
    if (i_ > -1 && i_ > i) {
      i = i_;
      n = numbers[k];
    }
  }
  return n;
}

function parse(str) {
  const first = getFirstNum(str);
  const last = getLastNum(str);
  return first * 10 + last;
}

function solve(input) {
  return input.map(solve).reduce((a, b) => a + b, 0);
}
