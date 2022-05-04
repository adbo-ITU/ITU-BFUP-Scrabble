// Usage: node generate-hand.js lettershere
const alphabet = "abcdefghijklmnopqrstuvwxyz";
const idOf = (letter) => alphabet.indexOf(letter.toLowerCase()) + 1;
const generate = (word) =>
  `[ ${word
    .split("")
    .map((l) => `${idOf(l)}u`)
    .join("; ")} ]`;

console.log(generate(process.argv[2]));
