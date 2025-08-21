module sdc.compile;
import lib.memory;
import sdc.grammar;
import sdc.codegen : CodeGen;
import sdc.symtable;
import sdc.semantic;
import sdc.parsetable : Prod, Action, ActionType;
import sdc.parser;

alias T = Token;
alias n = NonTerm;

union Args {
	Variable var;
	struct {
		Token type;
		void* value;
	}
}

union ParseData {
	ulong num;
	string str;
	struct {
		Token type;
		void* value;
	}
}

struct DataBuffer {
	ParseData[5] array;
	ubyte length;
	void add(ParseData data) {
		array[length] = data;
		length++;
	}
	ParseData pop() {
		length--;
		return array[length];
	}
}

struct Compiler {
	Parser parser;
	DataBuffer dataStack;
	ubyte args;
	List!Args argBuffer;
	SymbolTable symTable;
	CodeGen gen;
	Semantic sem;

	this(char* code)
	{
		parser = Parser(code);
		gen.initialize();
	}

	string curStr() => parser.tokenizer.curString;
	bool compile() {
		Action action = parser.next();

		with(ActionType)
		final switch(action.type) {
			case Shift: {
				shift(parser.tokenizer.current);
				parser.shift(action.state);
			} break;
			case Reduce: {
				parser.reduce(action.reduce);
				reduce(action.reduce.nonTerm, action.reduce.length);
			} break;
			case Accept: {
				return true;
			} break;
			case Error: {
				parser.error(action, parser.tokenizer.current);
			} break;
		}

		return false;
	}
	void shift(Token t) {
		switch(t) {
			default: break;
			case T.Ident: {
				dataStack.add(ParseData(str: curStr));
			} break;
			case T.tVoid, T.i32, T.i64, T.str: {
				dataStack.add(ParseData(type: t));
			} break;
			case T.NumLit: {
				import lib.string;
				StrNum num = strToNum(curStr);
				ParseData data;
				final switch(num.sign) {
					case 0: {
						if(num <= uint.max) {
							data.type = T.i32;
						}
						else {
							data.type = T.i64;
						}
					} break;
					case 1: {
						if(num <= byte.max) {

						}
					} break;
					case 2: assert(0, "Error: Number too large");
					case -1: assert(0, "Corrupt NumLiteral");
				}
				data.value = gen.toValue(num, data.type);
				dataStack.add(data);
			} break;
			case T.StrLit: {
				ParseData data;
				data.type = T.str;
				string str = curStr[1..$-1];
				data.value = gen.toValue(str);
				dataStack.add(data);
			} break;
		}
	}
	void reduce(NonTerm n, p_size length) {
		switch(n) {
			case n.CallArgs: {
				if(length < 1) {
					break;
				}
				ParseData expr = dataStack.pop();
				Args ar;
				ar.type = expr.type;
				ar.value = expr.value;
				argBuffer.add(ar);
				args++;
			} break;
			case n.FuncCall: {
				string ident = dataStack.pop.str;
				SymbolData* func = symTable.search(ident);
				if(!func) {
					assert(0, "Error: undefined identifier");
				}
				void*[128] funcArgs;
				foreach(i, c; argBuffer[$-args..$]) {
					funcArgs[i] = c.value;
				}
				ParseData callVal;
				callVal.value = gen.addCall(func.valueRef, funcArgs[0..args]);
				callVal.type = func.type;
				dataStack.add(callVal);
				args = 0;
			} break;
			case n.Args: {
				if(length <= 1) {
					break;
				}
				string name = dataStack.pop().str;
				Token type = dataStack.pop().type;
				argBuffer.add(Args(Variable(type, name)));
				args++;
			} break;
			case n.Stmnt: {
				dataStack = DataBuffer();
			} break;
			case n.FuncHeader: {
				FuncHeader fh;
				fh.args = args;
				args = 0;
				fh.decl.ident = dataStack.pop().str;
				fh.decl.type = dataStack.pop().type;
				if(symTable.search(fh.decl.ident)) {
					assert(0, "Duplicate name");
				}

				Variable[] argList = cast(Variable[])argBuffer[$-fh.args..$];
				sem.lastFunc = fh;
				void* funcRef = gen.addFunc(fh.decl, argList);
				SymbolData symData = SymbolData(fh.decl.ident, funcRef, fh.decl.type, argList);
				symTable.add(symData);
			} break;
			case n.VarDecl: {
				Variable var;
				var.ident = dataStack.pop().str;
				var.type = dataStack.pop().type;
				dataStack.add(ParseData(str: var.ident));
				if(symTable.search(var.ident)) {
					assert(0, "Error: Declaration shadows previous symbol");
				}
				void* varRef = gen.addVar(var);
				SymbolData data = SymbolData(name: var.ident, valueRef: varRef, type: var.type);
				symTable.add(data);
			} break;
			case n.AssignStmnt: {
				ParseData data = dataStack.pop();
				string ident = dataStack.pop().str;

				SymbolData* var = symTable.search(ident);
				if(!var) {
					assert(0, "Error: undefined identifier");
				}
				gen.addAssign(data.value, var.valueRef);

			} break;
			case n.ReturnStmnt: {
				switch(length) {
					case 1: {
						sem.checkRet(T.tVoid);
						gen.addRetVoid();
					} break;
					case 2: {
						ParseData data = dataStack.pop();
						sem.checkRet(data.type);
						gen.addRet(data.value);
					} break;
					default: assert(0);
				}
			} break;
			case n.Plus: {
				ParseData rExpr = dataStack.pop();
				ParseData lExpr = dataStack.pop();
				void* value = gen.addPlus(lExpr.value, rExpr.value);
				dataStack.add(ParseData(type: rExpr.type, value: value));
			} break;
			case n.Var: {
				string ident = dataStack.pop().str;
				SymbolData* var = symTable.search(ident);
				if(!var) {
					assert(0, "Error: undefined identifier");
				}
				void* value = gen.addLoad(var.valueRef, var.type);
				dataStack.add(ParseData(type: var.type, value: value));
			} break;
			case n.FuncDecl: {
				sem.lastFunc = FuncHeader();
			} break;
			case n.FuncExtern: {
				FuncHeader fh;
				fh.args = args;
				args = 0;
				fh.decl.ident = dataStack.pop().str;
				fh.decl.type = dataStack.pop().type;
				if(symTable.search(fh.decl.ident)) {
					assert(0, "Duplicate name");
				}

				Variable[] argList = cast(Variable[])argBuffer[$-fh.args..$];
				sem.lastFunc = fh;
				void* funcRef = gen.addFunc(fh.decl, argList, true);
				SymbolData symData = SymbolData(fh.decl.ident, funcRef, fh.decl.type, argList);
				symTable.add(symData);
			} break;
			default: break;
		}
	}
}