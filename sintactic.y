%{
  #include<stdio.h>
  #include<string.h>
  
  int yylex();
  int yyerror(const char *msg);
  int Correct=1;
  char msg[500];
  class TVAR
	{
	     char* nume;
	     int valoare;
	     TVAR* next;
	  
	  public:
	     static TVAR* head;
	     static TVAR* tail;

	     TVAR(char* n, int v = -1);
	     TVAR();
	     int exists(char* n);
             void add(char* n, int v = -1);
             int getValue(char* n);
	     void setValue(char* n, int v);
	};

	TVAR* TVAR::head;
	TVAR* TVAR::tail;

	TVAR::TVAR(char* n, int v)
	{
	 this->nume = new char[strlen(n)+1];
	 strcpy(this->nume,n);
	 this->valoare = v;
	 this->next = NULL;
	}

	TVAR::TVAR()
	{
	  TVAR::head = NULL;
	  TVAR::tail = NULL;
	}

	int TVAR::exists(char* n)
	{
	  TVAR* tmp = TVAR::head;
	  while(tmp != NULL)
	  {
	    if(strcmp(tmp->nume,n) == 0)
	      return 1;
            tmp = tmp->next;
	  }
	  return 0;
	 }

         void TVAR::add(char* n, int v)
	 {
	   TVAR* elem = new TVAR(n, v);
	   if(head == NULL)
	   {
	     TVAR::head = TVAR::tail = elem;
	   }
	   else
	   {
	     TVAR::tail->next = elem;
	     TVAR::tail = elem;
	   }
	 }

         int TVAR::getValue(char* n)
	 {
	   TVAR* tmp = TVAR::head;
	   while(tmp != NULL)
	   {
	     if(strcmp(tmp->nume,n) == 0)
	      return tmp->valoare;
	     tmp = tmp->next;
	   }
	   return -1;
	  }

	  void TVAR::setValue(char* n, int v)
	  {
	    TVAR* tmp = TVAR::head;
	    while(tmp != NULL)
	    {
	      if(strcmp(tmp->nume,n) == 0)
	      {
		tmp->valoare = v;
	      }
	      tmp = tmp->next;
	    }
	  }

	TVAR* ts = NULL;
%}
  
%union{ char* sir; int val; }  
  
%token TOK_PROG TOK_DECLARE TOK_BEGIN TOK_END TOK_INTEGER TOK_ASSIGN TOK_PLUS TOK_MINUS TOK_MULTIPLY TOK_DIVIDE TOK_LEFT TOK_RIGHT TOK_READ TOK_WRITE TOK_FOR TOK_DO TOK_TO TOK_ERROR
%token <val> TOK_NUMBER
%token <sir> TOK_VARIABLE

%type <val> factor
%type <sir> id_list
%type <val> term
%type <val> exp

%start S

%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY TOK_DIVIDE

%%
S: 
        |
	TOK_PROG prog_name TOK_DECLARE dec_list TOK_BEGIN stmt_list TOK_END '.'
	|
	error '.'  
	  { Correct =0; }
	;
prog_name : TOK_VARIABLE
    	   ;
dec_list: 
  |
  dec
  |
  dec_list ';' dec
  ;
dec: id_list ':' type  
	{
			
		char* sir=strtok($1,",");
		while(sir!=NULL)
		{
		 if(ts!=NULL)
		  {
			if(ts->exists(sir)==0)
			{
			  ts->add(sir);
      			 }
                  else 
			{
		 	  sprintf(msg, "%d:%d Eroare semantica: Declaratii multiple pentru variabila %s!",@1.first_line, @1.first_column, sir);
			  yyerror(msg);
			  YYERROR;
			 }
		}
		else 
			{
				ts=new TVAR();
				ts->add(sir);
				
			}
		 sir=strtok(NULL, ",");
		}

	}
    ; 
type: TOK_INTEGER
	;
id_list: 
  |
  TOK_VARIABLE
  |
  id_list ',' TOK_VARIABLE
	{
	  strcat($$,",");
	  strcat($$,$3);
	}

   ;
stmt_list: 
  |
   stmt
  |
   stmt_list ';' stmt
  ;
stmt: 
	|
	 assign
	|
	 read
	|
	 write
	|
	 for
	;
assign: TOK_VARIABLE TOK_ASSIGN exp
	{
		if(ts!=NULL)
			{
			  if(ts->exists($1)==0)
				{
		 	  	sprintf(msg, "%d:%d Eroare semantica: Variabila neclarata la atribuire %s!",@1.first_line, @1.first_column, $1);
			  	yyerror(msg);
			  	YYERROR;
				}
			else {
				ts->setValue($1,$3);
			     }

			}
		else 	{
		 	  sprintf(msg, "%d:%d Eroare semantica: Lista de variabile goala pentru atribuire!",@1.first_line, @1.first_column);
			  yyerror(msg);
			  YYERROR;
			 }
       }
    	;
exp:
  |
  term { $$=$1; }
  |
  exp TOK_PLUS term { $$=$1+$3; }
  |
  exp TOK_MINUS term  { $$=$1-$3; }
  ;  
term:
	|
	factor {$$=$1; }
	|
	term TOK_MULTIPLY factor {$$=$1*$3; }
	|
	term TOK_DIVIDE factor {
              		if($3==0)
				{
		 	  	sprintf(msg, "%d:%d Eroare semantica: Impartire la zero!",@1.first_line, @1.first_column);
			  	yyerror(msg);
			  	YYERROR;
				 }
			else {
			$$=$1/$3; }
		}
    ;
factor: 
  |
  TOK_VARIABLE 
	{
		if(ts!=NULL)
			{
				if(ts->exists($1)==0)
				{
		 	  	sprintf(msg, "%d:%d Eroare semantica: Variabila neclarata la atribuire %s!",@1.first_line, @1.first_column, $1);
			  	yyerror(msg);
			  	YYERROR;
				 }
				else
				{
					if(ts->getValue($1)!=-1)
				  		$$=ts->getValue($1);
					else
					{
		 	  			sprintf(msg, "%d:%d Eroare semantica: Variabila neinitializata la atribuire %s!",@1.first_line, @1.first_column, $1);
			  			yyerror(msg);
			  			YYERROR;
				 }
				}
			
			}
		else
			{
		 	  sprintf(msg, "%d:%d Eroare semantica: Lista de variabile goala!",@1.first_line, @1.first_column);
			  yyerror(msg);
			  YYERROR;
			 }
	}
  |
  TOK_NUMBER
  |
  TOK_LEFT exp TOK_RIGHT
    ;
read: TOK_READ TOK_LEFT id_list TOK_RIGHT
	{
	        char* sir=strtok($3,",");
		while(sir!=NULL)
		{
		 	if(ts->exists(sir)==1)
			{
			  //se va citi valoarea si se va atribui 
				//pentru functionare algoritm, se va atribui valoarea 0 in cazul nostru
			 ts->setValue(sir,0);
      			 }
                  	else 
			{
		 	  sprintf(msg, "%d:%d Eroare semantica: Variabila %s nu este declarata!",@1.first_line, @1.first_column, sir);
			  yyerror(msg);
			  YYERROR;
			 }
		 sir=strtok(NULL, ",");
		}
	}
        ;
write: TOK_WRITE  TOK_LEFT id_list TOK_RIGHT
	{
	        char* sir=strtok($3,",");
		while(sir!=NULL)
		{
		 	if(ts->exists(sir)==1)
			{
			  if(ts->getValue(sir)!=-1)
			     {//se va printa valoarea 
				}
			  else 
				{
		 	 	 sprintf(msg, "%d:%d Eroare semantica: Variabila %s nu este initializata!",@1.first_line, @1.first_column, sir);
			  	 yyerror(msg);
			  	 YYERROR;
				 }
      			 }
                  	else 
			{
		 	  sprintf(msg, "%d:%d Eroare semantica: Variabila %s nu este declarata!",@1.first_line, @1.first_column, sir);
			  yyerror(msg);
			  YYERROR;
			 }
		 sir=strtok(NULL, ",");
		}
	}
	;

for: TOK_FOR index_exp TOK_DO body
    { 
	 
    }
	;
index_exp: TOK_VARIABLE TOK_ASSIGN exp TOK_TO exp
		{
		if(ts!=NULL)
			{
				if(ts->exists($1)==0)
					{
                                        sprintf(msg, "%d:%d Eroare semantica: Variabila neclarata la atribuire %s!",@1.first_line, @1.first_column, $1);
			  		yyerror(msg);
			  		YYERROR;
				 	}
				else
				{
				  ts->setValue($1,$3);
				}
			
			}
		else
			{
		 	  sprintf(msg, "%d:%d Eroare semantica: Lista de variabile goala!",@1.first_line, @1.first_column);
			  yyerror(msg);
			  YYERROR;
			 }
	}
   ;
body: 
  |
  stmt
  |
  TOK_BEGIN stmt_list TOK_END
  ;
%%

int main()
{
	yyparse();
	
	if(Correct == 1)
	{
		printf("CORECTA\n");		
	}	

       return 0;
}

int yyerror(const char *msg)
{
	printf("Error: %s\n", msg);
	return 1;
}
