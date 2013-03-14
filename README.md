Sudoku in Haskell
=================

This project started as a simple sudoku solver written in Haskell.  

I wanted to try an approach more like a human would think about solving a sudoku board.  I did *not* want to just write a brute force solver.  The result is a solver that uses the basic constraints that I personally think about when solving a board.  Since the basic solver has been completed, there are a few other options for future improvements.

## Currently, it can do this ##

- Solve boards using basic row/column/box removal and uniqness checks. (Usually considered 'easy')
- Solve boards requiring use of exact cover logic. (Usually considered 'intermediate')
- Take input from standard in or from a list of puzzles in a file.
- Output solved board
- Generate basic random boards
- Solve and generate puzzles through a web service.

## What I would like it to eventually do ##

- Generate puzzles taking difficulty as parameter.
- Output list of steps, along with solved board.
- Add a chained logic heuristic to the solver. (Needed to solve 'difficult' puzzles)

## To use the program as it stands now ##

Clone the repo.  Make sure you have GHC 7.6.2 installed (haven't checked it with other versions yet).  In the cloned directory, do one of th following: 

### solve puzzle from sample file (refer to puzzle1 for format) ###

    runhaskell sudoku-test.hs solve < puzzles/sample-puzzles/puzzle1
    runhaskell sudoku-test.hs solve < puzzles/sample-puzzles/puzzle1 > out
    diff out puzzles/sample-solutions/solution1

*OR*

### solve puzzle that is already in the puzzle pool ###

    runhaskell sudoku-test.hs solve 1
    runhaskell sudoku-test.hs solve 5

*OR*

### generate random puzzle ###

    runhaskell sudoku-test.hs generate

### to run the web services 

    cabal build
    ./dist/build/sudoku/sudoku

There should now be a local server running on port 3000. Make the following example requests in a web browser:

    <server ip>:3000/generate
    <server ip>:3000/solve/<string representing puzzle> (e.g. the generated puzzle above)

*Lastly*

Questions/Comments/Concerns, just ask!