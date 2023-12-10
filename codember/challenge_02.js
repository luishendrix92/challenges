const input = "&###@&*&###@@##@##&######@@#####@#@#@#@##@@@@@@@@@@@@@@@*&&@@@@@@@@@####@@@@@@@@@#########&#&##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@##@@&";

function interpret(code, numVal = 0) {
  let output = "";
  const operations = {
    "#": (n) => n + 1,
    "@": (n) => n - 1,
    "*": (n) => n * n,
    "&": (n) => {
      output += String(n);
      return n;
    },
  };

  for (let token of code.split("")) {
    numVal = operations[token](numVal);
  }

  console.log(output);
}

interpret(input);
