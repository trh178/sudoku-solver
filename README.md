Sudoku in Haskell
=================

This project started as a simple sudoku solver written in Haskell.  

I wanted to try an approach more like a human would think about solving a sudoku board.  I did *not* want to just write a brute force solver.  The result is a solver that uses the basic constraints that I personally think about when solving a board.  Since the basic solver is pretty much complete, I have turned it into a web service and put it live on Heroku.

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

... now copy that puzzle that it generated (without the " " marks) ...

    <server ip>:3000/solve/<string representing puzzle> (paste it in there!)

### also live on Heroku

The service should be live here for anyone to use.

    stormy-brushlands-9203.herokuapp.com/generate
    stormy-brushlands-9203.herokuapp.com/solve/<string representing puzzle>

*Lastly*

Questions/Comments/Concerns, just ask!