Sudoku in Haskell
=================

This project started as a simple sudoku solver written in Haskell.  

I wanted to try an approach more like a human would think about solving a sudoku board.  I did *not* want to just write a brute force solver.  The result is a solver that uses the basic constraints that I personally think about when solving a board.  Since the basic solver has been completed, there are a few other options for future improvements.

## Currently, it can do this ##

- Solve boards using basic row/column/box removal and uniqness checks. (Usually considered 'easy')
- Solve boards requiring use of exact cover logic. (Usually considered 'intermediate')
- Take input from standard in or from a list of puzzles in a file.
- Output solved board

## What I would like it to eventually do ##

- Generate puzzles taking difficulty as parameter.
- Output list of steps, along with solved board.
- Add a chained logic heuristic to the solver. (Needed to solve 'difficult' puzzles)
- Add web service to return generated puzzle, or solved puzzle with steps.

## To use the program as it stands now ##

Clone the repo.  Make sure you have GHC 7.4.2 installed (haven't checked it with newer versions yet).  In the cloned directory, do one of th following: 

    runhaskell sudoku.hs < puzzle1
    runhaskell sudoku.hs < puzzle1 > out
    diff out solution1

*OR*

    runhaskell sudoku.hs puzzle-pool 1
    runhaskell sudoku.hs puzzle-pool 5


Questions/Comments/Concerns, just ask!