
# Scrabbλe

A one- or two-player implementation of Scrabble for teaching functional programming at the 
University of Brighton. It consists of a core library, and two clients (web and CLI) for playing
the game.

The Scrabble library is inspired by https://github.com/joshcough/Scrabble.

If you don't know the rules of the game you should begin by reading
them
[here](https://www.theukrules.co.uk/rules/children/games/scrabble.html). 

The emphasis is on explaining the process of developing a Haskell
project with several parts, aimed at students who have already learned
Haskell syntax and functional problem solving. Commonly used data
structures and libraries like `Data.Text`, maps, arrays and `aeson`
are explained, as well as monadic error checking and building a
WebSocket client to the game that shows how to use threads and other
concurrency abstractions safely.

Each chapter refines the initial solution, ending up with something
that is (hopefully) a clean, functional design that can easily be
extended.

Testing is a continuous topic, so new tests are added in each chapter.

+ [Chapter One](chapters/Chapter1.md): Getting started.
+ [Chapter Two](chapters/Chapter2.md): Players and the game.
+ [Chapter Three](chapters/Chapter3.md): Validating moves.
+ [Chapter Four](chapters/Chapter4.md): Playing the game.
+ [Chapter Five](chapters/Chapter5.md): Playing against the computer.
+ [Chapter Six](chapters/Chapter6.md): The CLI client.
+ [Chapter Seven](chapters/Chapter7.md): The web server and client.
+ [Chapter Eight](chapters/Chapter8.md): Configuration and conclusion.

## Disclaimer

Scrabble&trade; is a registered trademark. All intellectual property
rights in and to the game are owned in the U.S.A and Canada by Hasbro
Inc., and throughout the rest of the world by J.W. Spear & Sons
Limited of Maidenhead, Berkshire, England, a subsidiary of Mattel
Inc. This project and associated experiments are intended for teaching purposes
and are not associated with any of the owners.

