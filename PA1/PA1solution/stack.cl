(*
 * CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *)

class StackCommand {
   execute(node : StackNode) : StackNode {
      let errorNode : StackNode in {
         (new IO).out_string("Underfined execution");
	 errorNode;
      }
   };
   
   getNumber() : Int { 0 };
   getChar() : String { "empty" }; 
};

class Digit inherits StackCommand {
number : Int;   

   init(num : Int) : SELF_TYPE {
      {
	number <- num;
	self;
      }
   };
   execute(node : StackNode) : StackNode {
      node
   };
   
   getNumber() : Int { number };
   getChar() : String {(new A2I).i2a(number)};

};

class Plus inherits StackCommand {

   init() : SELF_TYPE {
      self
   };

   execute(node : StackNode) : StackNode {
      let n1 : StackNode <- node.getNext(),
	  n2 : StackNode <- n1.getNext(),
	  sum : Int,
	  tmp : StackNode
      in {
         if (not (isvoid n1)) then {
            if (not (isvoid n2)) then {
	       sum <- n1.getCommand().getNumber() + n2.getCommand().getNumber();
	       tmp <- (new StackNode).init((new Digit).init(sum), n2.getNext());
            }
	    else 0
	    fi;
         }
	 else 0
	 fi;
	 tmp;
      }
   };
   
   getChar() : String {
      "+"
   };
};

class Swap inherits StackCommand {

   init() : SELF_TYPE {
      self
   };

   execute(node : StackNode) : StackNode {
      let n1 : StackNode <- node.getNext(),
	  n2 : StackNode <- n1.getNext()
      in {
	 n1.changeNext(n2.getNext());
	 n2.changeNext(n1);
	 n2;
      }
   };
   
   getChar() : String {
      "s"
   };
};

class StackNode {
command : StackCommand;
next : StackNode;

   init(c : StackCommand, n : StackNode) : StackNode {
      {
	command <- c;
        next <- n;
        self;
      }
   };

   put(c : StackCommand) : StackNode {
      let node : StackNode in {
	 node <- (new StackNode).init(c, self);
	 node;
      }
   };

   getCommand() : StackCommand { command };
   getNext() : StackNode { next };
   changeNext(nextNode : StackNode) : StackNode { next <- nextNode };
};


class Main inherits IO {
stackTop : StackNode;

   printNode(stackTop : StackNode) : StackNode {
       let node : StackNode <- stackTop in {
          while (not (isvoid node)) loop
          {
             (new IO).out_string(node.getCommand().getChar());
             (new IO).out_string("\n");
             node <- node.getNext();
          }
          pool;
          node;
       }
   };

   pushCommand(command : StackCommand) : StackCommand {
      {
	if (isvoid stackTop) then {
	   let nil : StackNode in {
	      stackTop <- (new StackNode).init(command, nil);
	   };
        }
	else {
	   stackTop <- stackTop.put(command);
	}
	fi;
	command;
      }
   };

   stackOperation(input : String) : Object {
      {
	if (input = "+") then {
	   pushCommand((new Plus).init());
	}
	else
	   if (input = "s") then {
	      pushCommand((new Swap).init());
	   }
	   else
	      if (input = "e") then {
		 let node : StackNode <- stackTop in {
		    if (not (isvoid node)) then {
		       stackTop <- node.getCommand().execute(node);
		    }
                    else
                       0
		    fi;
		 };
	      }
	      else
		 if (input = "d") then {
		    printNode(stackTop);
		 }
		 else 
		    if (input = "x") then {
		       (new IO).out_string("Stop!\n");
		       abort();
                    }
		    else
		    {
		       pushCommand((new Digit).init((new A2I).a2i(input)));	
		    }
		    fi
		 fi
	      fi
	   fi
	fi;  
      } 
   };

   main() : Object {
      let input : String in {
         while true loop {
            out_string(">");
	    input <- in_string();
	    stackOperation(input);
         }
	 pool;
      }
   };

};

