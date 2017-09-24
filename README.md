# busybeaver

Straightforward Haskell implementation of the [Busy Beaver](https://en.wikipedia.org/wiki/Busy_beaver) problem.

Creates all n-state (2 default in the script) Turing machines and runs them for n steps (default 10 steps) filtering then those machies that halted.

The script then prints out the largest number of ones written on a tape (4 for a 2-state 2-symbol machine). Running the script for a higher state/symbol machine will take considerably longer with each step added.
