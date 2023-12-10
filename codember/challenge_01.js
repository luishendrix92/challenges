const fs = require('fs')

const contents = fs.readFileSync('message_01.txt', 'utf8');
const words = contents
  .trim()
  .split(' ')
  .reduce(function(frequencies, word) {
    if (frequencies.has(word)) {
      frequencies.set(word, frequencies.get(word) + 1);
    } else {
      frequencies.set(word, 1);
    }

    return frequencies;
  }, new Map());

let result = "";

for (let [word, count] of words.entries()) {
  result += word + count;
}

fs.writeFile('challenge_01_out.txt', result, console.log);
