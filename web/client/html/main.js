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
// MsgMoveAck (MoveAck (Left "Error!"))
//   <-> {"contents":{"Left":"Error!"},"tag":"MsgMoveAck"} CLIENT <- SERV
// MsgMoveAck (MoveAck (Right ([((0,0),(C,3)), ((0,1),(A,1)), ((0,2),(T,1))],42)))
//   <-> {"contents":{"Right":[[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]],42]}
//         ,"tag":"MsgMoveAck"}
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
	    setRack(d.contents.jaRack);
	    opponent.name = d.contents.jaOppName;
	    opponent.turn = (player.turn === "P1") ? "P2" : "P1";
	    serverMessage("Joined game!");
	    serverMessage("Your name is "+player.name+" and you are "+turnString(player.turn));
	    serverMessage("Your opponent is "+opponent.name);
	    $('#playerDisplayName').text(player.name+"'s score");
	    $('#playerDisplayScore').text(0);
	    $('#opponentDisplayName').text(opponent.name+"'s score");
	    $('#opponentDisplayScore').text(0);
	    displayRack();
	    break;
	case "MsgMove": //TODO REMOVE
	    serverMessage("Opponent played : "+d.contents.word+", "+d.contents.score);
	    addMoveToBoard(d.contents.word);
	    break;
	case "MsgHint":
	    doHints(d.contents);
	    break;
	case "MsgAnnounce":
	    ServerMessage("Announcement: "+d.contents);
	    break;
	case "MsgRack":
	    console.log("Received rack: "+d.contents);
	    setRack(d.contents);
	    break;
	case "MsgMoveAck":
	    console.log("Received move response.");
	    if (d.contents.Left != null) {
		serverMessage("Error! "+d.contents.Left);
		returnToRack();
	    } else {
		serverMessage(wordPutToWord(d.contents.Right[0])+": "+d.contents.Right[1]);
		if (isCurrentPlayer()) {
		    $('.tempInPlay').addClass("permInPlay");
		    $('.tempInPlay').removeClass("tempInPlay");
		    $('.emptyRackSpace div').attr('display','inline-block');
		    $('.emptyRackSpace').text('');
		    $('.emptyRackSpace').removeClass('emptyRackSpace');
		} else {
		    addMoveToBoard(d.contents.Right[0]);
		}
	    }
	    break;
	case "MsgScore":
	    console.log("Received score.");
	    var plSc = (player.turn==="P1") ? d.contents[0].theScore : d.contents[1].theScore;
	    var opSc = (player.turn==="P1") ? d.contents[1].theScore : d.contents[0].theScore;
	    console.log("Player score: "+plSc);
	    console.log("Opponent score: "+opSc);
	    $('#playerDisplayScore').text(plSc);
	    $('#opponentDisplayScore').text(opSc);
	    break;
	case "MsgTurn":
	    console.log("Received turn.");
	    turn = d.contents;
	    toggleActive(turn===player.turn);
	    if (isCurrentPlayer()) {
		serverMessage("It's your turn!");
	    }
	    break;
	default:
	    console.log("Couldn't match the tag: "+d.tag);
	} 
    }
}

// TODO Use as a little example of functional style in JS 
function wordPutToWord(wp) {
    return wp.map(x => x[1][0]).join('')
}

function isCurrentPlayer() {
    return turn===player.turn;
}

function addMoveToBoard(move) {
    move.forEach((el,i,a) => {
	console.log("el: "+el);
	var r = el[0][0];
	var c = el[0][1];
	$("div[data-row='"+r+"'][data-column='"+c+"']").text(el[1][0]);
	$("div[data-row='"+r+"'][data-column='"+c+"']").addClass("permInPlay");
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

function serverMessageRaw(msg) {
    $('#serverMessages').prepend(msg);
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

function setRack(letters) {
    console.log("Putting letters in rack: "+letters);
    player.rack = [];
    for (var t in letters) {
	var tile = { letter: letters[t] };
	player.rack.push(tile);
    }
    displayRack();
}

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

function removeSelectedTileFromRack() {
    $('.selected div').attr('display','none');
    $('.selected').addClass('emptyRackSpace');
    $('.selected').removeClass('selected');
    selected = false;
}

 //takes all tiles placed on the board from the current turn and returns them to that player's rack
function returnToRack() {
    $('.tempInPlay').text("");
    $('.tempInPlay').removeClass('tempInPlay');
    $('.emptyRackSpace div').attr('display','inline-block');
    $('.emptyRackSpace').removeClass('emptyRackSpace');
}

function pass() {
    if (confirm("Do you really want to pass?")) {
	returnToRack();
	var p = {"tag":"MsgPass"};
	socket.send(JSON.stringify(p));
    } else {
	return false;
    }
}

function swap() {}

function hints() {
    var h = {"contents":null,"tag":"MsgHint"};
    socket.send(JSON.stringify(h));
}

function doHints(hs) {
    var pBlock = $('<p></p>');
    hs.forEach(h => pBlock.prepend(h.join('')+'<br/>'));
    serverMessageRaw(pBlock);
    serverMessage("Hints:");
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

    //checks to make sure all conditions for playing a word have been met, then tallies the score, and resets for the next turn
    var submitWord = function() {
	var w = [];
	$('.tempInPlay').each(function (i) {
	    var r = parseInt(this.dataset.row);
	    var c = parseInt(this.dataset.column);
	    w.push([[r,c],[this.innerText,letterValue(this.innerText)]]);
	});
	sendMove(w);
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

