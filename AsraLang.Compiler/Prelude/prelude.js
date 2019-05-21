const println = console.log;
const add = (a) => (b) => a + b;
const subtract = (a) => (b) => a - b;
const multiply = (a) => (b) => a * b;
const divide = (a) => (b) => a / b;
const iffn = (cond) => (itrue) => (ifalse) => {
    if (cond()) {
        return itrue();
    } else {
        return ifalse();
    }
}
const eq = (a) => (b) => a === b;
