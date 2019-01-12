%{
	#include "y.tab.h"
	
	int lineNo=1;
	int colNo=1;
%}

%%
"program"	{ colNo+=7; return TOK_PROG; }
"var"		{ colNo+=3; return TOK_DECLARE; }
"begin"     	{ colNo+=5; return TOK_BEGIN; }
"end"       	{ colNo+=3; return TOK_END; }
"integer"	{ colNo+=7; return TOK_INTEGER; }
"div"		{ colNo+=3; return TOK_DIVIDE; }
"read"		{ colNo+=4; return TOK_READ; }
"write"		{ colNo+=5; return TOK_WRITE; }
"for"		{ colNo+=3; return TOK_FOR; }
"do"        	{ colNo+=2; return TOK_DO; }
"to" 		{ colNo+=2; return TOK_TO; }
[a-zA-Z][a-zA-Z0-9_]* {yylval.sir = new char[strlen(yytext)+1]; strcpy(yylval.sir,yytext); return TOK_VARIABLE; }
0|[1-9][0-9]* { yylval.val = atoi(yytext); return TOK_NUMBER; }
";"		{ colNo++; return ';'; }
"."		{ colNo++; return '.'; }
":"		{ colNo++; return ':'; }
","		{ colNo++; return ','; }
":="		{ colNo+=2; return TOK_ASSIGN; }
"+"		{ colNo++; return TOK_PLUS; }
"-" 		{ colNo++; return TOK_MINUS; }
"*"		{ colNo++; return TOK_MULTIPLY; }
"("		{ colNo++; return TOK_LEFT; }
")"		{ colNo++; return TOK_RIGHT; }
[ ]		{ colNo++;}
\r\n 		{ lineNo++; colNo=1; }
\n		{ lineNo++; colNo=1; }
.		{ colNo++; return TOK_ERROR; }	
%%
