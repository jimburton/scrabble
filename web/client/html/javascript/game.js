var playerName;

// Haskell <-> JSON datatype formats

// MsgJoin "Jim" <-> {"contents":"Jim","tag":"MsgJoin"} CLIENT -> SERV
//
// MsgJoinAck (JoinAck { theName="Jim", theRack=[A, B, C]}) <-> {"contents":{"theName":"Jim","theRack":["A","B","C"]},"tag":"MsgJoinAck"} CLIENT <- SERV
//
// MsgMove (Move {word=[((0,0), (C, 3)), ((0,1), (A,1)), ((0,2),(T,1))]})  <-> {"contents":{"word":[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]]},"tag":"MsgMove"} CLIENT <-> SERV
//
// MsgHint (Just [[C,A,T],[M,A,T]]) <-> {"contents":[["C","A","T"],["M","A","T"]],"tag":"MsgHint"} CLIENT <-> SERV
// MsgHint Nothing <-> {"contents":null,"tag":"MsgHint"}
//
// MsgPass <-> {"tag":"MsgPass"} CLIENT -> SERV
//
// MsgSwap [C, A, T] <-> {"contents":["C","A","T"],"tag":"MsgSwap"} CLIENT -> SERV
//
// MsgAnnounce "Hello, world!" <-> {"contents":"Hello, world!","tag":"MsgAnnounce"} CLIENT <- SERV
//
// MsgRack [A, B, C] <-> {"contents":["A","B","C"],"tag":"MsgRack"} CLIENT <- SERV
//
// MsgMoveRsp (MoveResponse (Left "Error")) <-> {"contents":{"Left":"Error"},"tag":"MsgMoveRsp"} CLIENT <- SERV
// MsgMoveRsp (MoveResponse (Right 42)) <-> {"contents":{"Right":42},"tag":"MsgMoveRsp"}
//
// MsgScore (Score {theTurn=P1,theScore=123},Score {theTurn=P2,theScore=456}) <-> {"contents":[{"theTurn":"P1","theScore":123},{"theTurn":"P2","theScore":456}],"tag":"MsgScore"} CLIENT <- SERV

function Client(socket) {
    socket.onopen = function () {
	// TODO: need a real way for players to enter their name
	var p = {"contents":"Jim","tag":"MsgJoin"}
	socket.send(JSON.stringify(p));
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
	    console.log("Received joinack. Name: "+d.contents.theName+", Rack: "+d.contents.theRack);
	    playerName = d.name;
	    break;
	case "MsgMove":
	    console.log("Received move. Word: "+d.word);
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

$(function(){
    socket = new WebSocket("ws://localhost:9160/")
    client = new Client(socket);
})

