const f1 = function (x) {
    console.log(x);
};

function f2(x) {
    console.log(x);
}

function f25(x): void {
    console.log(x);
}

const f3 = (x) => {
    console.log(x);
};

let f4 = (x, x2: number): void => {
    console.log(x);
};

let f5 = (x) => console.log(x);

let myAdd: (baseValue: number, increment: number) => number = function (
    x: number,
    y: number
): number {
    return x + y;
};

function myAdd2(x: number, y: number): number {
    return x + y;
}

function myAdd3(x: number, y: number) {
    return x + y;
}

f1(5);
f2(5);
f3(5);
f4(5, 6);
f5(5);

type GreetFunction = (a: string) => void;
type GreetFunction2 = (a) => void; // implicit any

interface Deck {
    suits: string[];
    cards: number[];
    a; // implicit any
    createCardPicker(value: number): () => string;
    createCardPicker2: (value: number) => () => string; // SAME AS LINE BEFORE IT!
    // (otherFunction: string): boolean; // TODO: Not supported yet
    "crazy function"?(onclick: (value: string, e: Event) => void): void;
    other: (value: string) => void;
}

for (; ; (x) => console.log(x)) {}
