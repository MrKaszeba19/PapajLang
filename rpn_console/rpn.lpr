program rpn;
uses Unit2, Unit5, Sysutils;

procedure show_version();
begin
     writeln('RPN Calculator. Version X.X.X (Leviathan)');
     writeln('Paul Lipkowski. January 15, 2018.');
     writeln('Since 11/24/2017. Proudly written in FPC. :)');
     writeln('');
end;

procedure show_help();
begin
     writeln('SYNTAX: rpn "quoted_rpn_expression" [flags]');
     writeln('');
     writeln('Run ''rpn expression'' to obtain info about making RPN expressions.');
     writeln('Run ''rpn operands [page]'' to obtain info about available operands.');
     writeln('Run ''rpn flags'' to obtain info about flags.');
end;

procedure show_flags();
begin
 	writeln('Flags: ');
 	writeln('    -e        Provides an output in scientific notation');
 	writeln('    -it       Provides an integer output (truncated)');
 	writeln('    -i, -ir   Provides an integer output (rounded)');
 	writeln('    -f        DEFAULT - Provides a decimal output');
 	writeln('              except for large numbers (more than 2E+49)');
 	writeln('    -f2       Provides a decimal output truncated to 2 digits');
 	writeln('    -fp       Provides a decimal output for all numbers');
 	writeln('              (with a constant precision of 15 digits)');
 	writeln('No flag provided - works on the ''-f'' flag by default.')
end;

procedure show_expressions;
begin
	writeln('EXPRESSIONS: ');
	writeln('Remember - the expression in console mode must be a "quoted" string');
	writeln('and each operation must be separated by 1 space.');
	writeln('');
	writeln('Type ''rpn operands'' to check out the available operands.');
	writeln('');
	writeln('EXAMPLES');
	writeln(' 6              -> 6');
	writeln(' 2+3            -> 2 3 +');
	writeln(' (12-6)/3.5     -> 12 6 - 3.5 /');
	writeln(' 5*(2-9)        -> 5 2 9 - *');
	writeln(' (12-4)/(4^0.5) -> 12 4 - 4 0.5 ^ /');
	writeln(' (2*PI)^E       -> 2 PI * E ^');
	writeln(' sin(5) + 1     -> 5 sin 1 +');
end;

procedure show_operands;
begin
	writeln('OPERANDS:');
	writeln('Page 1: Binary operands');
	writeln('Page 2: Unary operands');
	writeln('Page 3: Stack operations');
	writeln('Page 4: Constants');
	writeln('Page 5: Other');
	writeln;
	writeln('Type ''rpn operands [page_num]'' to obtain info about specific operands, e.g. ''rpn operands 1''');
end;

procedure show_operands1();
begin
	writeln('Binary operands model: (expr1) (expr2) (operand), e.g. 2 4 +');
    writeln('Available binary operands:');
    writeln('       +       -       *       /     div');
    writeln('       ^     pow    root     log     mod');
    writeln('Unary operands model: (expr0) (operand), e.g. 2 sin');
    writeln('Available unary operands:');
    writeln('     abs    sqrt     exp      ln       !    fact');
    writeln('     sin     cos     tan     csc     sec     cot');
    writeln('   trunc   round');
    writeln('Scan a value with a ''scan'' operand.');
    writeln('Available constants: ');
    writeln('      PI = ~3.1415926535897');
	writeln('      EU = ~2.7182818284590');
	writeln('      FI = ~1.6180339887498');
end;


//work in progress
{*
var
   x : String;
begin
	case ParamCount of
		0 : begin
			show_version();
     		writeln('No arguments provided - run ''rpn help''');
		end;
		1 : begin
			case ParamStr(1) of
     			'help' : begin
     				show_version();
     				show_help();
     			end;
     			'expression' : begin
     				show_version();
     				show_expressions();
     			end;
     			'operands' : begin
     				show_version();
     				show_operands();
     			end;
     			else begin
     				try
     					x := calc_parseRPN(ParamStr(1), '0.################');
        				writeln(x);
              		except
              			On E : Exception do
                 		begin
                      		writeln(E.ToString);
                 		end;
              		end;
     			end;
     		end;
		end;
		2 : begin
			case ParamStr(1) of
				'operands' : begin
     				show_version();
     				case ParamStr(2) of
     					
     				end;
     			end;
			end;
		end;
	end;
	*}



     //=============== OLDE =============================================================
var
	x : String;
begin
	case ParamCount of
		0 : begin
			show_version();
     		writeln('No arguments provided - run ''rpn help''');
		end;
    	1 : begin
     		case ParamStr(1) of
     			'help' : begin
     				show_version();
     				show_help();
 					//writeln('Run ''rpn flags'' to obtain info about flags.');

     			end;
     			'expression' : begin
     				show_version();
     				show_expressions();
     			end;
     			'operands' : begin
     				show_version();
     				show_operands1();
     			end;
     			else begin
            		try
     					x := calc_parseRPN(ParamStr(1), '0.################');
        				writeln(x);
            		except
            			On E : Exception do
                		begin
                			writeln(E.ToString);
                		end;
              		end;
     			end;
     		end;
        end;
        {*2 : begin
          try
        	case ParamStr(2) of
        		'-e' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		'-it' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		'-i' : begin
              x := calc_parseRPN(ParamStr(1));
              writeln(x);
        		end;
        		'-ir' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		'-f' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		'-f2' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		'-fp' : begin
        			x := calc_parseRPN(ParamStr(1));
        			writeln(x);
        		end;
        		else begin
        			show_version();
     				writeln('Unknown flag - run ''rpn help''');
        		end;
        	end;
          except
          On E : Exception do
             begin
                  writeln(E.ToString);
             end;
          end;
        end;*}
    end;
end.
