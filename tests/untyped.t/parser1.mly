%{

%}
%token AAA
%token EOF
%start <unit> main
%%
main: AAA EOF { };
