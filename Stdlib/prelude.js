const println = (a) => console.log(a.toString());
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
const not = (a) => !a;
const neq = (a) => (b) => a !== b;
const and = (a) => (b) => a && b;
const or = (a) => (b) => a || b;
const toStr = (a) => a.toString();
const trueVal = true;
const falseVal = false;
const newArray = (n) => (creator) => {
    const newarr = [];
    for (let i = 0; i < n; i++) {
        newarr.push(creator(i));
    }
    return newarr;
}
const forEach = (fun) => (arr) => arr.forEach(fun);
