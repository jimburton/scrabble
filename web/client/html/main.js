//////////////////////////////
// WebSocket code
//////////////////////////////

// Examples of Haskell to JSON datatypes forming the game protocol
//
// HASKELL           JSON                               DIRECTION
// MsgJoin ("Jim", True) <-> {"contents":["Jim",true],"tag":"MsgJoin"} CLIENT -> SERV
//
// MsgJoinAck (JoinAck { jaName="Jim", jaRack=[A, B, C], jaTurn=P1, jaOppName="Joe", jaOppTurn=P2})
//   <-> {"contents":{"jaName":"Jim","jaRack":["A","B","C"],"jaTurn":"P1","jaOppName":"Joe","jaOppTurn":"P2"}
//       ,"tag":"MsgJoinAck"}
//   CLIENT <- SERV
//
// MsgMove (Move {word=[((0,0), (C, 0)), ((0,1), (A,0)), ((0,2),(T,1))], blanks=[0,1]})
//  <-> {"contents":{"word":[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]],"blanks":[0,1]},"tag":"MsgMove"}
//  CLIENT <-> SERV
//
// MsgMoveAck (MoveAck (Left "Error!"))
//   <-> {"contents":{"Left":"Error!"},"tag":"MsgMoveAck"} CLIENT <- SERV
// MsgMoveAck (MoveAck (Right ( MoveResult [((0,0),(C,0)), ((0,1),(A,0)), ((0,2),(T,1))] -- WordPut
//                                         [[M,A,T]] -- additional words
//                                         [0,1]     -- indices of blanks
//                                         42 ) ) )  -- score
//   <-> {"contents":{"Right":
//                    {"mrScore":42,"mrWord":[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]],
//                    "mrAdditionalWords":[["M","A","T"]],"mrBlanks":[0,1]}},
//                    "tag":"MsgMoveAck"}
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
	console.log("closed web socket");
	alert("Server not available.");
    }
    socket.onerror = function (event) {
	console.log(event);
    }
    socket.onmessage = function (event) {
	var d = JSON.parse(event.data);
	console.log(d);
	switch(d.tag) {
	case "MsgJoinAck":
	    doMsgJoinAck(d);
	    break;
	case "MsgHint":
	    doHints(d.contents);
	    break;
	case "MsgAnnounce":
	    serverMessage(d.contents);
	    break;
	case "MsgRack":
	    console.log("Received rack: "+d.contents);
	    setRack(d.contents);
	    break;
	case "MsgMoveAck":
	    doMsgMoveAck(d);
	    break;
	case "MsgTurn":
	    doMsgTurn(d);
	    break;
	case "MsgEog":
	    console.log("Received Game Over");
	    serverMessage("Game over!")
	    toggleActive(false);
	default:
	    console.log("Couldn't match the tag: "+d.tag);
	} 
    }
}

// Respond to MsgJoinAck.
function doMsgJoinAck(msg) {
    player.name = msg.contents.jaName;
    player.turn = msg.contents.jaTurn;
    setRack(msg.contents.jaRack);
    opponent.name = msg.contents.jaOppName;
    opponent.turn = (player.turn === "P1") ? "P2" : "P1";
    serverMessage("Joined game!");
    serverMessage("Your name is "+player.name+" and you are "+turnString(player.turn));
    serverMessage("Your opponent is "+opponent.name);
    $('#playerDisplayName').text(player.name);
    $('#playerDisplayScore').text(0);
    $('#opponentDisplayName').text(opponent.name);
    $('#opponentDisplayScore').text(0);
    displayRack();
}

// Respond to MsgMoveAck.
function doMsgMoveAck(msg) {
    console.log("Received move response.");
    if (msg.contents.Left != null) {
	serverMessage("Error! "+msg.contents.Left);
	returnToRack();
    } else {
	var additionalWords = "";
	var wp = msg.contents.Right.mrWord;
	var w  = wordPutToWord(wp)
	var aw = msg.contents.Right.mrAdditionalWords;
	var bs = msg.contents.Right.mrBlanks;
	var sc = parseInt(msg.contents.Right.mrScore,10);
	var tn = msg.contents.Right.mrTurn;
	if (aw.length > 0) {
	    additionalWords = " ("+w+","+aw.map(a => a.join(''))+")";
	}
	serverMessage(wordPutToWord(wp)+": " + sc + additionalWords);
	if (isCurrentPlayer()) {
	    $('.tempInPlay').addClass("permInPlay");
	    $('.tempInPlay').removeClass("tempInPlay");
	    $('.emptyRackSpace div').attr('display','inline-block');
	    $('.emptyRackSpace').text('');
	    $('.emptyRackSpace').removeClass('emptyRackSpace');
	} else {
	    addMoveToBoard(wp);
	}
	var displayScore = (tn===player.turn) ? $('#playerDisplayScore') : $('#opponentDisplayScore');
	console.log(tn + "score: "+sc);
	displayScore.text(parseInt(displayScore.text(),10)+sc);
    }
}

// Respond to MsgTurn.
function doMsgTurn(msg) {
    console.log("Received turn.");
    turn = msg.contents;
    toggleActive(turn===player.turn);
    if (isCurrentPlayer()) {
	serverMessage("It's your turn!");
    }
}

// TODO Use as a little example of functional style in JS
// Convert a WordPut to a Word (list of letters).
function wordPutToWord(wp) {
    return wp.map(x => x[1][0]).join('')
}

// True if this client is the current player.
function isCurrentPlayer() {
    return turn===player.turn;
}

// Place a WordPut on the board.
function addMoveToBoard(move) {
    move.forEach((el,i,a) => {
	var r = el[0][0];
	var c = el[0][1];
	$("div[data-row='"+r+"'][data-column='"+c+"']").text(el[1][0]);
	$("div[data-row='"+r+"'][data-column='"+c+"']").addClass("permInPlay");
    });
}

// Drag handler.
function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.innerText);
    ev.dataTransfer.setData("theID", ev.target.id);
}

// Turn the form inputs on or off.
function toggleActive(p) {
    if(p) {
	$('.button').removeAttr("disabled");
    } else {
	$('.button').attr("disabled", true);
    }
}

// Make tiles in rack selectable.
function makeTilesSelectable() {
    $('.tileBox').removeClass('selected');
    $(this).addClass('selected');
    selected = true;
}

// Get the current turn as a longer string.
function turnString(turn) {
    if (turn==="P1") {
	return "Player 1";
    } else {
	return "Player 2";
    }
}

// Write a message to the serverMessages div inside a <p> tag.
function serverMessage(msg) {
    $('#serverMessages').prepend("<p>"+msg+"</p>");
}

// Write a message to the serverMessages div without any <p> tag.
function serverMessageRaw(msg) {
    $('#serverMessages').prepend(msg);
}

// Join a game.
function join(name, isAi) {
    var p = {"contents":[name,isAi],"tag":"MsgJoin"};
    socket.send(JSON.stringify(p));
}

// Send a move to the server.
function sendMove(wp, blanks) {
    if (wp.length>0) {
	// each blank is of the form [index,letter] and
	// each element in the wordput is of the form [[row,col],[letter,score]]
	blanks.forEach(b => wp[b[0]][1][0] = b[1]);
	var m = {"contents":{"word":wp,"blanks":blanks.map(b => b[0])},"tag":"MsgMove"};
	socket.send(JSON.stringify(m));
    }
}

// Set the char in a string (used with blanks).
function setCharAt(str,index,chr) {
    if(index > str.length-1) return str;
    return str.substring(0,index) + chr + str.substring(index+1);
}

// Set the rack of the player.
function setRack(letters) {
    player.rack = [];
    for (var t in letters) {
	var tile = { letter: letters[t] };
	player.rack.push(tile);
    }
    displayRack();
}

// Display the rack of the player.
function displayRack() {
    $("[id^='playerTile']").attr('display','inline-block');
    //add tiles to the rack display
    for (j = 0; j < player.rack.length; j++) {
	if (player.rack[j] != null) {
	    var c = (player.rack[j].letter === 'Blank') ? '_' : player.rack[j].letter;
            $('#playerTile'+j).text(c);
	}
    }
}

// Remove the selected tile from the rack.
function removeSelectedTileFromRack() {
    $('.selected div').attr('display','none');
    $('.selected').addClass('emptyRackSpace');
    $('.selected').removeClass('selected');
    selected = false;
}

 //takes all tiles placed on the board from the current turn and returns them to that player's rack.
function returnToRack() {
    $('.tempInPlay').text("");
    $('.tempInPlay').removeClass('tempInPlay');
    $('.emptyRackSpace div').attr('display','inline-block');
    $('.emptyRackSpace').addClass('playerTile');
    $('.emptyRackSpace').removeClass('emptyRackSpace');
}

// Pass the move.
function pass() {
    if (confirm("Do you really want to pass?")) {
	returnToRack();
	var p = {"tag":"MsgPass"};
	socket.send(JSON.stringify(p));
	serverMessage("You passed.");
    } else {
	return false;
    }
}

// Take a move by swapping tiles.
function swap() {
    var len = $('.selected').length;
    if (len > 0) {
	var msg = (len===1) ? "Do you want to swap the letter " : "Do you want to swap the letters ";
	var letters = $('.selected').text();
	if (confirm(msg+letters+"?")) {
	    doSwap(letters);
	    $('.selected').removeClass('selected');
	}
    }
}

// Send the swap message to the server.
function doSwap(letters) {
    var sw = {"contents":letters.split(''),"tag":"MsgSwap"};
    socket.send(JSON.stringify(sw));
}

// Send the hints message to the server.
function hints() {
    var h = {"contents":null,"tag":"MsgHint"};
    socket.send(JSON.stringify(h));
}

// Display hints.
function doHints(hs) {
    var pBlock = $('<p></p>');
    hs.forEach(h => pBlock.prepend(h.join('')+'<br/>'));
    serverMessageRaw(pBlock);
    serverMessage("Hints:");
}

// Find the letters on the board that come before a word that has been played.
function prefixOfWord(word) {
    // elements of word are [[r,c],[l,letterValue(l)]]
    var prefix = [];
    if (word.length > 1) {
	var p1 = word[0][0];
	var p2 = word[1][0];
	if (p1[0] < p2[0]) { // it's a vertical word
	    prefix = takeLetters([p1[0]-1,p1[1]], (([r,c]) => [r-1,c]));
	} else {
	    prefix = takeLetters([p1[0],p1[1]-1], (([r,c]) => [r,c-1]));
	}
    }
    return prefix;
}

// Find the letters on the board that come after a word that has been played.
function suffixOfWord(word) {
    var suffix = [];
    if (word.length > 1) {
	var p1 = word[0][0];
	var p2 = word[1][0];
	var last = word[word.length - 1][0];
	if (p1[0] < p2[0]) { // it's a vertical word
	    suffix = takeLetters([last[0]+1,last[1]], (([r,c]) => [r+1,c]));
	} else {
	    suffix = takeLetters([last[0],last[1]+1], (([r,c]) => [r,c+1]));
	}
    }
    return suffix;
}

// Retrieve a list of tiles on the board starting at pos and moving in
// the direction determined by transFunction.
function takeLetters(pos,transFunction) {
    var next = transFunction(pos);
    var l = getTile(pos);
    if (onBoard(pos) && l != null) {
	return [[pos,[l,letterValue(l)]]].concat(takeLetters(next,transFunction));
    } else {
	return [];
    }
}

// Fill the gaps in a word that is played on either side of some existing tiles.
function fillGaps(word) {
    var filled = word;
    if (word.length > 1) {
	var p1 = word[0][0];
	var p2 = word[1][0];
	var vertical = p1[0] < p2[0]; // it's a vertical word
	var fillOffset = 0;
	var pos;
	var lastPos = word[0][0];
	for (i=1;i<word.length;i++) {
	    pos = word[i][0];
	    if (vertical && pos[0] != lastPos[0]+1) { // there's a vertical gap
		for(j=1;j<pos[0]-lastPos[0];j++) {
		    var l = getTile([lastPos[0]+j,lastPos[1]]);
		    filled.splice(i+fillOffset,0,[[lastPos[0]+j,lastPos[1]],[l,letterValue(l)]]);
		    fillOffset++;
		}
	    } else if (pos[1] != lastPos[1]+1) { // there's a horizontal gap
		for(j=1;j<pos[1]-lastPos[1];j++) {
		    var l = getTile([lastPos[0],lastPos[1]+j]);
		    filled.splice(i+fillOffset,0,[[lastPos[0],lastPos[1]+j],[l,letterValue(l)]]);
		    fillOffset++;
		}
	    }
	    lastPos = pos;
	}
    }
    return filled;
}

// True if pos is on the board.
function onBoard(pos) {
    return pos[0] >= 0 && pos[0] < 14 && pos[1] >= 0 && pos[1] < 14; 
}

// Get the tile letter at a position on the board.
function getTile(pos) {
    var l = $("div[data-row='"+pos[0]+"'][data-column='"+pos[1]+"']").text();
    if (l != '') {
	return l;
    } else {
	return null;
    }
}

// Return the value of a letter.
function letterValue(l) {
    createBag();
    var selectedTile = tileBag.find(function(tile1) {
        return tile1.letter === l;
    });
    return selectedTile.score;
}

// Create the bag of tiles.
function createBag() {
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
        { letter: "Z", score: 10, count: 1 },
	{ letter: "_", score: 0, count: 2 }
    ];
}

// Connect to the websocket server.
$(function(){
    socket = new WebSocket("ws://localhost:9160/")
    client = new Client(socket);
})

//////////////////////////////////////////
// Game init code
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
    
    //////////////////
    // Bonus squares
    //////////////////
    var createBonusSquares = function() {
	bonusSquares = [{"row":0, "cols": [{"c":0,"b":"W3"}, {"c":3,"b": "L2"}
					   , {"c":7,"b": "W3"}, {"c":11,"b": "L2"}
					   , {"c":14,"b": "W3"}]}
			, {"row":1, "cols": [{"c":1, "b": "W2"}, {"c":5,"b": "L3"}
					     , {"c":9,"b": "L3"}, {"c":13,"b": "W2"}]}
			, {"row":2, "cols": [{"c":2,"b":"W2"}, {"c":6,"b":"L2"}
					     , {"c":8,"b":"L2"}, {"c":12,"b":"W2"}]}
			, {"row":3, "cols": [{"c":0,"b":"L2"}, {"c":3,"b":"W2"}
					     , {"c":7,"b":"L2"}, {"c":11,"b":"W2"}
					     , {"c":14,"b":"L2"}]}
			, {"row":4, "cols": [{"c":4,"b":"W2"}, {"c":10,"b":"W2"}]}
			, {"row":5, "cols": [{"c":1,"b":"L3"}, {"c":5,"b":"L3"}
					     , {"c":9,"b":"L3"}, {"c":13,"b":"L3"}]}
			, {"row":6, "cols": [{"c":2,"b":"L2"}, {"c":6,"b":"L2"}
					     , {"c":8,"b":"L2"}, {"c":12,"b":"L2"}]}
			, {"row":7, "cols": [{"c":0,"b":"W3"}, {"c":3,"b":"L2"}
					     , {"c":7,"b":"W2"}, {"c":11,"b":"L2"}
					     , {"c":14,"b":"W3"}]}
			, {"row":8, "cols": [{"c":2,"b":"L2"}, {"c":6,"b":"L2"}
					     , {"c":8,"b":"L2"}, {"c":12,"b":"L2"}]}
			, {"row":9, "cols": [{"c":1,"b":"L3"}, {"c":5,"b":"L3"}
					     , {"c":9,"b":"L3"}, {"c":13,"b":"L3"}]}
			, {"row":10, "cols": [{"c":4,"b":"W2"}, {"c":10,"b":"W2"}]}
			, {"row":11, "cols": [{"c":0,"b":"L2"}, {"c":3,"b":"W2"}
					      , {"c":7,"b":"L2"}, {"c":11,"b":"W2"}
					      , {"c":14,"b":"L2"}]}
			, {"row":12, "cols": [{"c":2,"b":"W2"}, {"c":6,"b":"L2"}
					      , {"c":8,"b":"L2"}, {"c":12,"b":"W2"}]}
			, {"row":13, "cols": [{"c":1,"b":"W2"}, {"c":5,"b":"L3"}
					      , {"c":9,"b":"L3"}, {"c":13,"b":"W2"}]}
			, {"row":14, "cols": [{"c":0,"b":"W3"}, {"c":3,"b":"L2"}
					      , {"c":7,"b":"W3"}, {"c":11,"b":"L2"}
					      , {"c":14,"b":"W3"}]}];
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
		    if (player.turn===turn) {
			var b = this;
			ev.preventDefault();
			ev.dataTransfer = ev.originalEvent.dataTransfer;
			console.log('dropped: '+ev.dataTransfer.getData("text"));
			$(b).text(ev.dataTransfer.getData("text"));
			$(b).addClass("tempInPlay");
			var pt = ev.dataTransfer.getData("theID");
			console.log("Dropped from "+pt);
			$('#'+pt).addClass("emptyRackSpace");
		    }
		});
		$(newBox).on('dragover',function(ev){
		    ev.preventDefault();
		});
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

    //checks to make sure all conditions for playing a word have been met, then tallies the score, and resets for the next turn
    var submitWord = function() {
	var w = [];
	$('.tempInPlay').each(function (i) {
	    var r = parseInt(this.dataset.row);
	    var c = parseInt(this.dataset.column);
	    var l = this.innerText
	    w.push([[r,c],[l,letterValue(l)]]);
	});
	var pre = prefixOfWord(w);
	var suf = suffixOfWord(w);
	var word = fillGaps(pre.concat(w).concat(suf));
	var blanks = [];
	word.forEach((l, i) => {
	    if (l[1][0]==='_') {
		var b = prompt("Enter the letter to replace the blank at position "
			       +(i+1)+" in "+word.join(''));
		blanks.push([i,b]);
	    }});
	sendMove(word, blanks);
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
	var isAi = $('#aiCheck').is(':checked');
	thename = $('.playerName').val();
        if (thename !== "") {
            $('.instructions').fadeOut(1000);
            players[0].name = thename;
	    join(thename, isAi);
        } else {
            alert("You must enter a name.");
        }
    }

    //--------BEGIN FUNCTION DEPLOYMENT---------

    //run these the first time only
    resetGame();
    createBag();
    createBoard();
    createLetterKey();

    //---------BEGIN EVENT LISTENERS---------------

    //visually marks a tile as 'selected' when clicked
    $(document.body).on('click', '.tileBox', function(ev) {
	if ( !ev.ctrlKey ) {
	    $('.tileBox').removeClass('selected');
	}
        $(this).addClass('selected');
        selected = true;
    });

    //adds a tile to the board if nothing occupies that space already
    $(document.body).on('click', '.box', function() {
	if ($('.selected').length > 1) {
	    alert("Select a single tile to place on the board");
	} else if (selected && player.turn===turn) {
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

    //runs the returnToRack function when the button is clicked
    $('.backToRack').click(returnToRack);

    //runs the submitWord function when the submit button is clicked
    $('.submitWord').click(submitWord);

    //refreshes the rack when the refresh tiles buttons is clicked
    $('.hints').click(hints);

    //returns all tiles to the rack when the return to rack button is clicked
    $('.swap').click(swap);

    //opens or closes the letter key when the tab is clicked
    $('.tab').click(tabClick);

    //starts a new game when the play again button is clicked
    $('.pass').click(pass);

});

