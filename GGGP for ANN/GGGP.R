library("gramEvol")
INPUT = 2
OUTPUT = 3

grammar <- list(
  S = gsrule("<a><h>/<z>"),
  a = gsrule(replicate(INPUT, "n")),
  z = gsrule(replicate(OUTPUT, "n")),
  h = gsrule("<h><h>", "/<n>"),
  n = gsrule("n<n>", "n")
)

grammarDef <- CreateGrammar(grammar)
print(grammarDef)

