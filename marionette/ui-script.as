// -*- java -*-

trace ("HELLO!!!");

disc_mc.addEventListener (MouseEvent.MOUSE_MOVE, function (ev) {
			      trace (ev);
// 			      handleMouseMove (ev.localX, ev.localY);
			  });

// [MouseEvent
// type="mouseMove"
// bubbles=true
// cancelable=false
// eventPhase=2
// localX=-19 localY=95
// stageX=241 stageY=297.95
// relatedObject=null
// ctrlKey=false altKey=false shiftKey=false delta=0]

stage.addEventListener (KeyboardEvent.KEY_DOWN, function (ev) {
			      trace ("keyDown: " + ev);
			  });

// [KeyboardEvent
// type="keyDown"
// bubbles=true
// cancelable=false
// eventPhase=2
// charCode=103 keyCode=71
// keyLocation=0 ctrlKey=false altKey=false shiftKey=false]
