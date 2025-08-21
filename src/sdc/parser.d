module sdc.parser;
import lib.memory;
import sdc.grammar;
import sdc.parsetable;
import sdc.lexer;

struct ParseResult {
	ActionType type;
	union {
		Token term;
		NonTerm nonTerm;
	}
	p_size length;
}

struct Parser {
	List!p_size stateStack;
	Tokenizer tokenizer;
	immutable ParseTable ptable = makePTable(NonTerm.File);

	this(char* code) {
		tokenizer = Tokenizer(code);
		tokenizer.next();
		stateStack.add(0);
	}

	Action next()
	{
		Token curToken = tokenizer.current;
		p_size state = stateStack[$-1];
		Action action = ptable.actionTable[state][curToken];
		return action;
	}
	void shift(p_size state) {
		stateStack.add(state);
		tokenizer.next;
	}
	void reduce(Prod prod) {
		stateStack.pop(prod.length);
		stateStack.add(ptable.gotoTable[stateStack[$-1]][prod.nonTerm]);
	}
	void error(Action action, Token curToken) {
		import lib.io;
		write("Line ", tokenizer.locs, ", Expected: ");
		p_size[1] startState = [action.state];
		List!p_size stateList = startState;
		for(ubyte i; i < stateList.length; i++) {
			foreach(Token i_tok, Action ac; ptable.actionTable[stateList[i]])
			{
				if(ac.type == ActionType.Error) {
					continue;
				}
				if(ac.type == ActionType.Reduce) {
					//stateList.add(_ptable.gotoTable[state][ac.reduce.nonTerm]);
					continue;
				}
				write(i_tok, ' ');
			}
		}
		writeln("Got: ", curToken, " in ", tokenizer.curString);
		assert(0, "Parsing error");
	}
}