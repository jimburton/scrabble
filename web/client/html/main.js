//////////////////////////////
// WebSocket code
//////////////////////////////

// Examples of Haskell to JSON datatypes forming the game protocol
//
// HASKELL           JSON                               DIRECTION
// MsgJoin "Jim" <-> {"contents":"Jim","tag":"MsgJoin"} CLIENT -> SERV
//
// MsgJoinAck (JoinAck { jaName="Jim", jaRack=[A, B, C], jaTurn=P1, jaOppName="Joe", jaOppTurn=P2})
//   <-> {"contents":{"jaName":"Jim","jaRack":["A","B","C"],"jaTurn":"P1","jaOppName":"Joe","jaOppTurn":"P2"}
//       ,"tag":"MsgJoinAck"}
//   CLIENT <- SERV
//
// MsgMove (Move {word=[((0,0), (C, 3)), ((0,1), (A,1)), ((0,2),(T,1))]})
//  <-> {"contents":{"word":[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]]},"tag":"MsgMove"}
//  CLIENT <-> SERV
//
// MsgHint (Just [[C,A,T],[M,A,T]])
//   <-> {"contents":[["C","A","T"],["M","A","T"]],"tag":"MsgHint"} CLIENT <-> SERV
// MsgHint Nothing <-> {"contents":null,"tag":"MsgHint"}
//
// MsgPass <-> {"tag":"MsgPass"} CLIENT -> SERV
//
// MsgSwap [C, A, T] <-> {"contents":["C","A","T"],"tag":"MsgSwap"} CLIENT -> SERV
//
// MsgAnnounce "Hello, world!" <-> {"contents":"Hello, world!","tag":"MsgAnnounce"}
//   CLIENT <- SERV
//
// MsgRack [A, B, C] <-> {"contents":["A","B","C"],"tag":"MsgRack"} CLIENT <- SERV
//
// MsgMoveRsp (MoveResponse (Left "Error"))
//   <-> {"contents":{"Left":"Error"},"tag":"MsgMoveRsp"} CLIENT <- SERV
//   MsgMoveRsp (MoveResponse (Right 42)) <-> {"contents":{"Right":42},"tag":"MsgMoveRsp"}
//
// MsgScore (Score {theTurn=P1,theScore=123},Score {theTurn=P2,theScore=456})
//   <-> {"contents":[{"theTurn":"P1","theScore":123},{"theTurn":"P2","theScore":456}],"tag":"MsgScore"}
//   CLIENT <- SERV


var player   = {name: "",score: 0,rack: [],turn:""};
var opponent = {name: "",score: 0,turn: ""};
var turn     = "";
var socket;
var client;

function Client(socket) {
    socket.onopen = function () {
    }
    socket.onclose = function () {
      console.log("closed web socket")
    }
    socket.onerror = function (event) {
      console.log(event)
    }
    socket.onmessage = function (event) {
	var d = JSON.parse(event.data);
	console.log(d);
	switch(d.tag) {
	case "MsgJoinAck":
	    player.name = d.contents.jaName;
	    player.turn = d.contents.jaTurn;
	    addToRack(d.contents.jaRack);
	    opponent.name = d.contents.jaOppName;
	    opponent.turn = (player.turn === "P1") ? "P2" : "P1";
	    serverMessage("Joined game!");
	    serverMessage("Your name is "+player.name+" and you are "+turnString(player.turn));
	    serverMessage("Your opponent is "+opponent.name);
	    $("#playerTitle").text(player.name+"'s Tiles");
	    displayRack();
	    turn="P1";
	    toggleActive(player.turn==="P1");
	    break;
	case "MsgMove":
	    console.log("Received move. Word: "+d.contents.word);
	    addMoveToBoard(d.contents.word);
	    break;
	case "MsgHint":
	    console.log("Received hints: "+d.contents);
	    break;
	case "MsgAnnounce":
	    console.log("Received announcement: "+d.contents);
	    break;
	case "MsgRack":
	    console.log("Received rack: "+d.contents);
	    break;
	case "MsgMoveRsp":
	    console.log("Received move response.");
	    if (d.contents.Left != null) {
		console.log("Response was an error: "+d.contents.Left);
	    } else {
		console.log("Response was a score: "+d.contents.Right);
	    }
	    break;
	case "MsgScore":
	    console.log("Received score.");
	    console.log(d.contents[0].theTurn+": "+d.contents[0].theScore);
	    console.log(d.contents[1].theTurn+": "+d.contents[1].theScore);
	    break;
	default:
	    console.log("Couldn't match the tag: "+d.tag);
	} 
    }
}

function addMoveToBoard(move) {
    move.forEach((el,i,a) => {
	console.log("el: "+el);
	var r = el[0][0];
	var c = el[0][1];
	$("div[data-row='"+r+"'][data-column='"+c+"']").text(el[1][0]);
    });
}

function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.innerText);
}

function toggleActive(p) {
    if(p) {
	$('.button').removeAttr("disabled");
    } else {
	$('.button').attr("disabled", true);
    }
}

function makeTilesSelectable() {
    $('.tileBox').removeClass('selected');
    $(this).addClass('selected');
    selected = true;
}


function turnString(turn) {
    if (turn==="P1") {
	return "Player 1";
    } else {
	return "Player 2";
    }
}

function serverMessage(msg) {
    $('#serverMessages').prepend("<p>"+msg+"</p>");
}

function join(name) {
    var p = {"contents":name,"tag":"MsgJoin"};
    socket.send(JSON.stringify(p));
}

function sendMove(w) {
    var m = {"contents":{"word":w},"tag":"MsgMove"}
    console.log(JSON.stringify(m));
    socket.send(JSON.stringify(m));
}

function addToRack(letters) {
    for (var t in letters) {
	var tile = { letter: letters[t] };
	player.rack.push(tile);
    }
}

function displayRack() {
    console.log("Loading into rack: "+player.rack);
    //adds tiles to the player's rack array
    for (j = 0; j < player.rack.length; j++) {
	if (player.rack[j] != null) {
	    var c = (player.rack[j].letter === 'Blank') ? '_' : player.rack[j].letter;
	    console.log('Setting #playerTile'+j+" to "+c);
            $('#playerTile'+j+' div').text(c);
	}
    }
}

function removeSelectedTileFromRack() {
    $('.selected div').attr('display','none');
    $('.selected').addClass('emptyRackSpace');
    $('.selected').removeClass('selected');
    selected = false;
}

$(function(){
    socket = new WebSocket("ws://localhost:9160/")
    client = new Client(socket);
})

//////////////////////////////////////////
// Game code
//////////////////////////////////////////

$(function() {

    //-------BEGIN FUNCTION DECLARATIONS------

    var shuffledBag = [];
    var tempBag = [];
    var selected = false;
    var tabCounter = 0;
    var firstTurn = true;
    var sameColumn = true;
    var sameRow = true;
    var players = [];
    var bonusSquares;

    //-------BEGIN FUNCTION DECLARATIONS------

    var resetGame = function() {
            //global variables
            shuffledBag = [];
            tempBag = [];
            selected = false;
            tabCounter = 0;
            firstTurn = true;
            sameColumn = true;
            sameRow = true;

            //player objects
            players =[{
                name: "",
                score: 0,
                rack: []
            },
            {
                name: "",
                score: 0,
                rack: []
            }]
        }
    
    var createBag = function() {
        tileBag = [
            { letter: "E", score: 1, count: 12 },
            { letter: "A", score: 1, count: 9 },
            { letter: "I", score: 1, count: 9 },
            { letter: "O", score: 1, count: 8 },
            { letter: "N", score: 1, count: 6 },
            { letter: "R", score: 1, count: 6 },
            { letter: "T", score: 1, count: 6 },
            { letter: "L", score: 1, count: 4 },
            { letter: "S", score: 1, count: 4 },
            { letter: "U", score: 1, count: 4 },
            { letter: "D", score: 2, count: 4 },
            { letter: "G", score: 2, count: 3 },
            { letter: "B", score: 3, count: 2 },
            { letter: "C", score: 3, count: 2 },
            { letter: "M", score: 3, count: 2 },
            { letter: "P", score: 3, count: 2 },
            { letter: "F", score: 4, count: 2 },
            { letter: "H", score: 4, count: 2 },
            { letter: "V", score: 4, count: 2 },
            { letter: "W", score: 4, count: 2 },
            { letter: "Y", score: 4, count: 2 },
            { letter: "K", score: 5, count: 1 },
            { letter: "J", score: 8, count: 1 },
            { letter: "X", score: 8, count: 1 },
            { letter: "Q", score: 10, count: 1 },
            { letter: "Z", score: 10, count: 1 }
        ]
    }

    //////////////////
// Bonus squares
//////////////////
    var createBonusSquares = function() {
	bonusSquares = [{"row":0, "cols": [{"c":0,"b":"W3"}, {"c":3,"b": "L2"}, {"c":7,"b": "W3"}, {"c":11,"b": "L2"}, {"c":14,"b": "W3"}]}
			, {"row":1, "cols": [{"c":1, "b": "W2"}, {"c":5,"b": "L3"}, {"c":9,"b": "L3"}, {"c":13,"b": "W2"}]}
			, {"row":2, "cols": [{"c":2,"b":"W2"}, {"c":6,"b":"L2"}, {"c":8,"b":"L2"}, {"c":12,"b":"W2"}]}
			, {"row":3, "cols": [{"c":0,"b":"L2"}, {"c":3,"b":"W2"}, {"c":7,"b":"L2"}, {"c":11,"b":"W2"}, {"c":14,"b":"L2"}]}
			, {"row":4, "cols": [{"c":4,"b":"W2"}, {"c":10,"b":"W2"}]}
			, {"row":5, "cols": [{"c":1,"b":"L3"}, {"c":5,"b":"L3"}, {"c":9,"b":"L3"}, {"c":13,"b":"L3"}]}
			, {"row":6, "cols": [{"c":2,"b":"L2"}, {"c":6,"b":"L2"}, {"c":8,"b":"L2"}, {"c":12,"b":"L2"}]}
			, {"row":7, "cols": [{"c":0,"b":"W3"}, {"c":3,"b":"L2"}, {"c":7,"b":"W2"}, {"c":11,"b":"L2"}, {"c":14,"b":"W3"}]}
			, {"row":8, "cols": [{"c":2,"b":"L2"}, {"c":6,"b":"L2"}, {"c":8,"b":"L2"}, {"c":12,"b":"L2"}]}
			, {"row":9, "cols": [{"c":1,"b":"L3"}, {"c":5,"b":"L3"}, {"c":9,"b":"L3"}, {"c":13,"b":"L3"}]}
			, {"row":10, "cols": [{"c":4,"b":"W2"}, {"c":10,"b":"W2"}]}
			, {"row":11, "cols": [{"c":0,"b":"L2"}, {"c":3,"b":"W2"}, {"c":7,"b":"L2"}, {"c":11,"b":"W2"}, {"c":14,"b":"L2"}]}
			, {"row":12, "cols": [{"c":2,"b":"W2"}, {"c":6,"b":"L2"}, {"c":8,"b":"L2"}, {"c":12,"b":"W2"}]}
			, {"row":13, "cols": [{"c":1,"b":"W2"}, {"c":5,"b":"L3"}, {"c":9,"b":"L3"}, {"c":13,"b":"W2"}]}
			, {"row":14, "cols": [{"c":0,"b":"W3"}, {"c":3,"b":"L2"}, {"c":7,"b":"W3"}, {"c":11,"b":"L2"}, {"c":14,"b":"W3"}]}];
    }

    //create physical game board
    var createBoard = function() {
        for (i = 0; i < 15; i++) {
            var newRow = $('<div class="row"></div>');
            for (j = 0; j < 15; j++) {
                var newBox = $('<div class="box"></div>');
                newBox.attr("data-row", i);
                newBox.attr("data-column", j);
		$(newBox).on('drop dragdrop',function(ev){
		    ev.preventDefault();
		    console.log('dropped: '+ev);
		    var keys = [];
		    for(var key in ev){
			keys.push(key);
		    }
		    for(var i;i<keys.length;i++) {
			var k = keys[i];
			console.log(k+": "+ev.k);
		    }
		});
		$(newBox).on('dragover',function(ev){
		    ev.preventDefault();
		})
                newRow.append(newBox);
            }
            $('.gameBoard').append(newRow);
        }
	createBonusSquares();
	bonusSquares.forEach((r,i,a) => {
	    r.cols.forEach((c,j,b) => {
		$("div[data-row='"+r.row+"'][data-column='"+c.c+"']").addClass(c.b);
	    });
	});
    }

    //creates the letter values key
    var createLetterKey = function() {
        tileBag.forEach(function(tile) {
            var newListing = $('<li>');
            newListing.text(tile.letter + ": " + tile.score + " points");
            $('.letterValueList').append(newListing);
        });
    }

    //creates a temporary bag giving each tile its own array item
    var createTileBag = function() {
        while (tileBag.length > 0) {
            tempBag.push(tileBag[0]);
            tileBag[0].count--;
            if (tileBag[0].count === 0) {
                tileBag.shift();
            }
        }
    }

    //shuffles the bag
    var shuffleBag = function() {
        while (tempBag.length > 0) {
            var rndm = Math.floor(tempBag.length * Math.random());
            shuffledBag.push(tempBag[rndm]);
            tempBag.splice(rndm, 1);
        }
    }

    //tallies up score and declares a winner
    var endGame = function() {
        //deducts remaining tile values from each player
        players.forEach(function(player) {
            player.rack.forEach(function(tile) {
                player.score -= tile.score;
            })
        })

        //re-initializes the game board
        $('.tempInPlay').text("");
        $('.permInPlay').text("");
        $('.permInPlay').removeClass('permInPlay');
        $('.playerOneTile').remove();
        $('.playerTwoTile').remove();
        $('.playerOneTiles').hide();
        $('.playerOneTitle').hide();
        $('.playerTwoTiles').show();
        $('.playerTwoTitle').show();
        $('.playerOneTilesRow').hide();
        $('.playerTwoTilesRow').hide();

        //winning conditions
        if (players[0].score > players[1].score) {
            alert(players[0].name + " is the winner!\nFinal score:\n" + players[0].name + ": " + players[0].score + " points\n" + players[1].name + ": " + players[1].score + " points");
        } else if (players[0].score < players[1].score) {
            alert(players[1].name + " is the winner!\nFinal score:\n" + players[0].name + ": " + players[0].score + " points\n" + players[1].name + ": " + players[1].score + " points");
        } else {
            alert("It's a tie game!");
        }

        //sets up for another game
        createBag();
        resetGame();
        $('.container').hide();
        $('.letterValuesBox').hide();
        $('.playAgain').show();
    };

    //determines whose turn it is
    var isItPlayerOnesTurn = function() {
        if ($('.playerOneTiles').css("display") !== 'none') {
            return true;
        } else {
            return false;
        }
    }

    //given a player, will fill their rack with up to 7 tiles
    var loadRackX = function(player) {
        //adds tiles to the player's rack array until it's either at 7 or until the bag is empty
        for (i = player.rack.length; i < 7; i++) {
            if (shuffledBag.length > 0) {
                player.rack.push(shuffledBag[0]);
                shuffledBag.shift();
                if (player === players[0]) {
                    var newTileBox = $('<div class="playerOneTile tileBox" id="tileBox"></div>');
                    $('.playerOneTilesRow').append(newTileBox);
                } else {
                    var newTileBox = $('<div class="playerTwoTile tileBox"></div>');
                    $('.playerTwoTilesRow').append(newTileBox);
                }
            }
        }

        //updates the DOM to display the text on the player's rack
        if (player === players[0]) {
            for (j = 0; j < $('.playerOneTile').length; j++) {
                $('.playerOneTile').eq(j).text(player.rack[j].letter);
            }
        } else {
            for (j = 0; j < $('.playerTwoTile').length; j++) {
                $('.playerTwoTile').eq(j).text(player.rack[j].letter);
            }
        }
    }

    //returns the score value of a given tile
    var letterValue = function(l) {
        //var selectedLetter = tile.text();
        createBag();
        var selectedTile = tileBag.find(function(tile1) {
            return tile1.letter === l;
        });
        return selectedTile.score;
    }

    //given 2 tiles, returns the direction the 'adjacent' one is to the 'original' one
    var direction = function(original, adjacent) {
        if (original.attr('data-row') < adjacent.attr('data-row')) {
            return "bottom";
        } else if (original.attr('data-row') > adjacent.attr('data-row')) {
            return "top";
        } else if (original.attr('data-column') < adjacent.attr('data-column')) {
            return "right";
        } else {
            return "left";
        }
    }

    //tallies the score from a given turn and adds it to the player's total
    var tallyScore = function(player) {
        var counter = 0;
        $(".counted").removeClass('counted');

        //checks each new tile added
        for (var i = 0; i < $('.tempInPlay').length; i++) {
            player.score += letterValue($('.tempInPlay').eq(i));
            var adjacentToMe = adjacentTiles(parseInt($('.tempInPlay').eq(i).attr('data-row')), parseInt($('.tempInPlay').eq(i).attr('data-column')), true, true, true, true);

            //iterates along each adjacent tile for each new tile added to the board
            for (j = 0; j < adjacentToMe.length; j++) {
                if (adjacentToMe[j].hasClass('permInPlay') && !(adjacentToMe[j].hasClass('counted'))) {
                    player.score += letterValue(adjacentToMe[j]);
                    adjacentToMe[j].addClass('counted');
                    if ((sameRow && (adjacentToMe[j].attr('data-column') === $('.tempInPlay').eq(i).attr('data-column'))) || (sameColumn && (adjacentToMe[j].attr('data-row') === $('.tempInPlay').eq(i).attr('data-row')))) {
                        player.score += letterValue($('.tempInPlay').eq(i));
                    }
                    var stop = false;

                    //checks each direction of the current tile and keeps adding score values until edge of word or edge of board is reached
                    if (direction($('.tempInPlay').eq(i), adjacentToMe[j]) === "left") {
                        var dirCounter = 0;
                        while (stop === false) {
                            var nextOne = adjacentTiles(parseInt(adjacentToMe[j].attr('data-row')), parseInt(adjacentToMe[j].attr('data-column')) - dirCounter, false, false, true, false);
                            if (nextOne.length > 0 && nextOne[0].hasClass("permInPlay") && !(nextOne[0].hasClass('counted'))) {
                                player.score += letterValue(nextOne[0]);
                                nextOne[0].addClass("counted");
                                dirCounter++;
                            } else {
                                stop = true;
                            }
                        }
                    }
                    if (direction($('.tempInPlay').eq(i), adjacentToMe[j]) === "right") {
                        var dirCounter = 0;
                        while (stop === false) {
                            var nextOne = adjacentTiles(parseInt(adjacentToMe[j].attr('data-row')), parseInt(adjacentToMe[j].attr('data-column')) + dirCounter, false, false, false, true);
                            if (nextOne.length > 0 && nextOne[0].hasClass("permInPlay") && !(nextOne[0].hasClass('counted'))) {
                                player.score += letterValue(nextOne[0]);
                                nextOne[0].addClass("counted");
                                dirCounter++;
                            } else {
                                stop = true;
                            }
                        }
                    }
                    if (direction($('.tempInPlay').eq(i), adjacentToMe[j]) === "top") {
                        var dirCounter = 0;
                        while (stop === false) {
                            var nextOne = adjacentTiles(parseInt(adjacentToMe[j].attr('data-row')) - dirCounter, parseInt(adjacentToMe[j].attr('data-column')), true, false, false, false);
                            if (nextOne.length > 0 && nextOne[0].hasClass("permInPlay") && !(nextOne[0].hasClass('counted'))) {
                                player.score += letterValue(nextOne[0]);
                                nextOne[0].addClass("counted");
                                dirCounter++;
                            } else {
                                stop = true;
                            }
                        }
                    }
                    if (direction($('.tempInPlay').eq(i), adjacentToMe[j]) === "bottom") {
                        var dirCounter = 0;
                        while (stop === false) {
                            var nextOne = adjacentTiles(parseInt(adjacentToMe[j].attr('data-row')) + dirCounter, parseInt(adjacentToMe[j].attr('data-column')), false, true, false, false);
                            if (nextOne.length > 0 && nextOne[0].hasClass("permInPlay") && !(nextOne[0].hasClass('counted'))) {
                                player.score += letterValue(nextOne[0]);
                                nextOne[0].addClass("counted");
                                dirCounter++;
                            } else {
                                stop = true;
                            }
                        }
                    }
                }
            }
        }

        //permanently removes tiles in play from player's rack
        if (isItPlayerOnesTurn()) {
            while (counter < player.rack.length) {
                if ($('.playerOneTile').eq(counter).css("display") === 'none') {
                    player.rack.splice(counter, 1);
                    $('.playerOneTile').eq(counter).remove();
                } else {
                    counter++;
                }
            }
        } else {
            while (counter < player.rack.length) {
                if ($('.playerTwoTile').eq(counter).css("display") === 'none') {
                    player.rack.splice(counter, 1);
                    $('.playerTwoTile').eq(counter).remove();
                } else {
                    counter++;
                }
            }
        }
    }

    //ends a turn and starts a new one
    var turnX = function() {
        $('.playerOneTilesRow').hide();
        $('.playerTwoTilesRow').hide();
        if (shuffledBag.length > 0) {
            loadRack(players[0]);
            loadRack(players[1]);
        }

        //conditions for going to the "end game" scenario
        if (players[0].rack.length === 0 || players[1].rack.length === 0) {
            endGame();
        } else {
            $('.playerOneTiles').toggle();
            $('.playerOneTitle').toggle();
            $('.playerTwoTiles').toggle();
            $('.playerTwoTitle').toggle();

            $('.playerOneScore').text(players[0].score);
            $('.playerTwoScore').text(players[1].score);
            $('.remainingTiles').text(shuffledBag.length);
        }
    }

    //takes all tiles placed on the board from the current turn and returns them to that player's rack
    var returnToRack = function() {
        $('.tempInPlay').text("");
        $('.tempInPlay').removeClass('tempInPlay');
	$('.emptyRackSpace div').attr('display','inline-block');
	$('.emptyRackSpace').removeClass('emptyRackSpace');
    }

    //returns the DOM elements of all adjacent tiles; parameters determine which directions are checked
    var adjacentTiles = function(row, column, justAbove, justBelow, justLeft, justRight) {
        var allAdjacents = [];
        if (justAbove && row > 0) {
            var above = $("[data-row=\'" + (row - 1) + "\'][data-column=\'" + column + "\']");
            allAdjacents.push(above);
        }
        if (justBelow && row < 14) {
            var below = $("[data-row=\'" + (row + 1) + "\'][data-column=\'" + column + "\']");
            allAdjacents.push(below);
        }
        if (justLeft && column > 0) {
            var left = $("[data-row=\'" + row + "\'][data-column=\'" + (column - 1) + "\']");
            allAdjacents.push(left);
        }
        if (justRight && column < 14) {
            var right = $("[data-row=\'" + row + "\'][data-column=\'" + (column + 1) + "\']");
            allAdjacents.push(right);
        }
        return allAdjacents;
    }

    //returns whether or not all newly placed tiles are all in a line (either vertically or horizontally)
    var correctOrientation = function() {
        sameColumn = true;
        sameRow = true;
        var row = $('.tempInPlay').eq(0).attr('data-row');
        var column = $('.tempInPlay').eq(0).attr('data-column');
        for (i = 0; i < $('.tempInPlay').length; i++) {
            if ($('.tempInPlay').eq(i).attr('data-column') !== column) {
                sameColumn = false;
            }
            if ($('.tempInPlay').eq(i).attr('data-row') !== row) {
                sameRow = false;
            }
        }
        return (sameRow || sameColumn);
    }

    //returns whether at least one newly placed tile is touching a tile already on the board
    var permanentAdjacent = function() {
        for (i = 0; i < $('.tempInPlay').length; i++) {
            var adjacentToMe = adjacentTiles(parseInt($('.tempInPlay').eq(i).attr('data-row')), parseInt($('.tempInPlay').eq(i).attr('data-column')), true, true, true, true);
            for (j = 0; j < adjacentToMe.length; j++) {
                if (adjacentToMe[j].hasClass('permInPlay')) {
                    return true;
                }
            }
        }
        return false;
    }

    //returns whether or not all newly placed tiles are adjacent to each other
    var allTouching = function() {
        var counter = 0;
        if (sameRow) {
            for (l = 0; l < $('.tempInPlay').length; l++) {
                var adjacentToMe = adjacentTiles(parseInt($('.tempInPlay').eq(l).attr('data-row')), parseInt($('.tempInPlay').eq(l).attr('data-column')), false, false, true, true);
                for (x = 0; x < adjacentToMe.length; x++) {
                    if (!(adjacentToMe[x].hasClass('permInPlay') || adjacentToMe[x].hasClass('tempInPlay'))) {
                        counter++;
                    }
                }
                if (parseInt($('.tempInPlay').eq(l).attr('data-column')) === 0 || parseInt($('.tempInPlay').eq(l).attr('data-column')) === 14) {
                    counter++;
                }
            }

        } else if (sameColumn) {
            for (l = 0; l < $('.tempInPlay').length; l++) {
                var adjacentToMe = adjacentTiles(parseInt($('.tempInPlay').eq(l).attr('data-row')), parseInt($('.tempInPlay').eq(l).attr('data-column')), true, true, false, false);
                for (x = 0; x < adjacentToMe.length; x++) {
                    if (!(adjacentToMe[x].hasClass('permInPlay') || adjacentToMe[x].hasClass('tempInPlay'))) {
                        counter++;
                    }
                }
                if (parseInt($('.tempInPlay').eq(l).attr('data-row')) === 0 || parseInt($('.tempInPlay').eq(l).attr('data-row')) === 14) {
                    counter++;
                }
            }
        }
        if (counter > 2) {
            return false;
        }
        return true;
    }

    //checks to make sure all conditions for playing a word have been met, then tallies the score, and resets for the next turn
    var submitWord = function() {
        correctOrientation();
	var w = [];
	$('.tempInPlay').each(function (i) {
	    var r = parseInt(this.dataset.row);
	    var c = parseInt(this.dataset.column);
	    w.push([[r,c],[this.innerText,letterValue(this.innerText)]]);
	});
	sendMove(w);
    }

    //takes all tiles in the player's rack and returns them to the bag
    var refreshTiles = function() {
        returnToRack();
        if (isItPlayerOnesTurn()) {
            players[0].rack.forEach(function(tile) {
                shuffledBag.splice(Math.floor(Math.random() * shuffledBag.length), 0, tile);
            });
            while (players[0].rack.length > 0) {
                players[0].rack.pop();
            }
            $('.playerOneTile').remove();
        } else {
            players[1].rack.forEach(function(tile) {
                shuffledBag.splice(Math.floor(Math.random() * shuffledBag.length), 0, tile);
            });
            while (players[1].rack.length > 0) {
                players[1].rack.pop();
            }
            $('.playerTwoTile').remove();
        }
        turn();
    }

    //opens or closes the letter key on right side of the screen
    var tabClick = function() {
        if (tabCounter % 2 === 0) {
            $('.letterValuesBox').removeClass('slideRight');
            $('.letterValuesBox').addClass('slideLeft');
        } else {
            $('.letterValuesBox').removeClass('slideLeft');
            $('.letterValuesBox').addClass('slideRight');
        }
        tabCounter++;
    }

    //checks that each player entered a name and removes the instruction screen and initializes the board screen
    var startGame = function() {
	console.log($('.playerName'));
	console.log($('.playerName').val());
	thename = $('.playerName').val();
        if (thename !== "") {
            $('.instructions').fadeOut(1000);
            players[0].name = thename;
	    join(thename);
            //players[1].name = $('.playerTwoName').val();
            //$('.playerOneDisplayName').text(players[0].name + "\'s Score");
            //$('.playerTwoDisplayName').text(players[1].name + "\'s Score");
            //turn();
        } else {
            alert("You must enter a name.");
        }
    }

    //resets everything visually once the 'play again' button is clicked
    var startingProcedure = function() {
        $('.playAgain').hide();
        $('.container').show();
        $('.letterValuesBox').show();
        $('.instructions').fadeIn();
        createTileBag();
        //shuffleBag();
    }

    //--------BEGIN FUNCTION DEPLOYMENT---------

    //run these the first time only
    resetGame();
    createBag();
    //createBonusSquares();
    createBoard();
    createLetterKey();
    startingProcedure();

    //---------BEGIN EVENT LISTENERS---------------

    //visually marks a tile as 'selected' when clicked
    $(document.body).on('click', '.tileBox', function() {
        $('.tileBox').removeClass('selected');
        $(this).addClass('selected');
        selected = true;
    });

    //adds a tile to the board if nothing occupies that space already
    $(document.body).on('click', '.box', function() {
        if (selected && player.turn===turn) {
            if (!($(this).hasClass('permInPlay')) && (!$(this).hasClass('tempInPlay'))) {
                $(this).text($('.selected').text());
                $(this).addClass('tempInPlay')
                removeSelectedTileFromRack();
                $('.selected').removeClass('selected');
                selected = false;
            }
        }
    });

    //starts the game when the start button is clicked
    $('.start').click(startGame);

    //runs the submitWord function when the submit button is clicked
    $('.submitWord').click(submitWord);

    //refreshes the rack when the refresh tiles buttons is clicked
    $('.refreshTiles').click(refreshTiles);

    //returns all tiles to the rack when the return to rack button is clicked
    $('.backToRack').click(returnToRack);

    //opens or closes the letter key when the tab is clicked
    $('.tab').click(tabClick);

    //starts a new game when the play again button is clicked
    $('.playAgain').click(startingProcedure);

});

