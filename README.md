This is a project skeleton for the homework from the Haskell class being taught
as part of PDXFUNC. Instructions:
- Clone this repo with `git clone https://github.com/enolan/pdxfunc-cis194.git`.
  On Windows, I think [GitHub Desktop](https://desktop.github.com/) is the
  easiest option. On Mac OS, ask someone else. Maybe homebrew?
- [Get Stack](https://docs.haskellstack.org/en/stable/README/)
- Run in this directory: `stack test`. You should mostly failures. (Some tests
  pass by accident.)
- Edit the files in `src`. `Week1.hs` is for week 1, `Week2.hs` for week 2, etc.
  Fill in the sections that say "YOUR CODE GOES HERE". `stack test --file-watch`
  will re-run the tests every time you save. To only run the tests for a
  particular week, use `stack test --test-arguments "-p \"Week 3\""`. Replace 3
  with whichever week you want.
