/*****
 * File parser.mly: 「お茶」言語のための構文定義
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****/

%{
  open Ast
%}

/* お茶言語で用いられる字句の種類：これらの字句の正規表現は lexer.mll 
   に与えられる */

%token <bool> BOOL         /* 論理定数 */
%token <int> INT           /* 自然数定数 */
%token <char> CHAR         /* 文字定数 */
%token <string> STRING     /* 文字列定数 */
%token <string> IDENT      /* 識別子（変数名など） */
%token SEMI COMMA BRA ARB
%token AND OR              /* &&, || */
%token EQQ NEQ             /* ==, <> */
%token LT GT LE GE         /* <, >, <=, >= */
%token PLUS MINUS          /* +, - */
%token TIMES DIV MOD       /* *, /, %, mod */
%token EQ                  /* = */
%token ASSIGN              /* := */
%token IF THEN ELSE ENDIF  /* if, then, else, end-if */
%token WHILE DO ENDWHILE   /* while, do, end-while */
%token FOR TO ENDFOR       /* for, to, end-for */
%token PRINT DUMP          /* print, show_status */
%token EOF                 /* ファイルの終端を表す記号 */

/* 文法の曖昧性を除去するための、結合性に関する優先順位の定義。下に宣
   言されたものほど優先度が高い */

%left SEMI                 /* 順次実行 */
%left OR                   /* 論理和 */
%left AND                  /* 論理積 */
%left EQQ NEQ              /* 同値性比較 */
%left LT GT LE GE          /* 大小比較演算 */
%left PLUS MINUS           /* 加減算 */
%left TIMES DIV MOD        /* 乗除算 */
%nonassoc UNARY            /* 単項演算 */

%start otemae                /* お茶言語の文脈自由文法の開始記号は otemae */
%type <Ast.command> otemae
%%
otemae:
    command EOF             { $1 };

command:
    IDENT EQ expr           { Define ($1, $3) }
  | IDENT ASSIGN expr       { Set ($1, $3) }
  | IDENT BRA expr ARB ASSIGN expr { A_set ($1, $3, $6) }
  | IF expr THEN command ENDIF { If ($2, $4, Skip) }
  | IF expr THEN command ELSE command ENDIF { If ($2, $4, $6) }
  | WHILE expr DO command ENDWHILE { While ($2, $4) }
  | FOR IDENT EQ expr TO expr DO command ENDFOR { For ($2, $4, $6, $8) }
  | command SEMI command    { match $1 with Seq commands ->
                                Seq (commands @ [$3])
                              | _ -> Seq [$1; $3] }
/*  | PRINT IDENT             { Print $2 } */
  | PRINT expr              { Print $2 }
  | DUMP                    { Dump };

expr:
    BOOL                    { Bool $1 }
  | INT                     { Int $1 }
  | CHAR                    { Char $1 }
  | STRING                  { String $1 }
  | IDENT                   { Ident $1 }
  | unary expr %prec UNARY  { Unary ($1, $2) }
  | expr binary expr        { Binary ($2, $1, $3) }
  | BRA ARB                 { Array [] }
  | BRA exprs ARB           { Array $2 }
  | IDENT BRA expr ARB      { A_get ($1, $3) };

exprs:
    expr             { [ $1 ] }
  | expr COMMA exprs { $1 :: $3 }

unary:
    MINUS { "-" }

binary:
    TIMES { "*" } | DIV { "/" } | MOD { "%" } | PLUS { "+" } | MINUS { "-" }
  | EQ { "=" }
  | EQQ { "=" } | NEQ { "<>" }
  | LT { "<" } | GT { ">" } | LE { "<=" } | GE { ">=" }
  | AND   { "&&" } | OR { "||" }
