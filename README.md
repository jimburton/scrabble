
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
structures and libraries like `Data.Text`, maps, arrays, `aeson` and
`lens` are explained, as well as monadic error checking and building a
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

## Having a quick game

There are two front ends to try out the game. Each of which allows you
to play against the computer or against another person. Start a game
in the terminal like this:

```
$ cabal run scrabble
Enter 1P or 2P
1p
Enter name of player
bob

**********************************************
bob (0)
O, E, O, H, N, N, E
1, 1, 1, 4, 1, 1, 1
**********************************************


**********************************************
Haskell (0)
D, Z, B, L, I, A, _
2, 10, 3, 1, 1, 1, 0
**********************************************

  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 7|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 8|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 9|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
10|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
------------------------------------------------


**********************************************
bob (0)
O, E, O, H, N, N, E
1, 1, 1, 4, 1, 1, 1
**********************************************
Enter WORD ROW COL DIR[H/V]:
hone 7 7 v

  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 7|  |  |  |  |  |  |  | H|  |  |  |  |  |  |  |
 8|  |  |  |  |  |  |  | O|  |  |  |  |  |  |  |
 9|  |  |  |  |  |  |  | N|  |  |  |  |  |  |  |
10|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
------------------------------------------------

14

```

To start a web-based game you need to start the server then start one
or two clients. Start the server with `cabal run
scrabble-server`. Then start the first client by opening
`web/client/html/index.html` in a browser. Select the checkbox 
for a one-player game or leave it unchecked and open the page again in another 
tab for a two player game.

<img src="/chapters/images/webgame.png" alt="Playing Scrabble on the web" width="500px" />

If you want to run clients on a network, start the client server with
`cabal run scrabble-client`. This serves the web client on 
http://localhost:8000/index.html. To change the hostname, port, etc, see
`/etc/scrabble.conf`.

## Disclaimer

Scrabble&trade; is a registered trademark. All intellectual property
rights in and to the game are owned in the U.S.A and Canada by Hasbro
Inc., and throughout the rest of the world by J.W. Spear & Sons
Limited of Maidenhead, Berkshire, England, a subsidiary of Mattel
Inc. This project and associated experiments are intended for teaching purposes
and are not associated with any of the owners.

