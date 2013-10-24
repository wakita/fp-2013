(*****
 * File lexer.mll: 「お茶」言語のための字句定義
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****)


{
open Parser

let line_no = ref 0
} 
rule token = parse
  [' ' '\t' ]         { token lexbuf }	(* 空白は読みとばす *)
| '\n'                { incr line_no; token lexbuf }  (* 行末記号も読みとばす *)
| eof                 { EOF }  (* ファイル終端記号 *)

| '#' [^ '\n']+       { token lexbuf }  (* # 以後はコメントとして読みとばす *)

| "true" { BOOL true } | "false" { BOOL false }  (* 論理定数 *)

| ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) } (* 自然数定数 *)

| '\'' _ '\'' { CHAR ((Lexing.lexeme lexbuf).[1]) }  (* 文字定数 *)

| '"' [^ '"']* '"'    {  (* 文字列定数 *)
  let s = Lexing.lexeme lexbuf in
  STRING (String.sub s 1 (String.length s - 2)) }

    (* 区切り記号 *)
| ';' { SEMI } | ',' { COMMA } | '[' { BRA } | ']' { ARB }

    (* 比較と代入 *)
| '=' { EQ } | ":=" { ASSIGN }

    (* 演算子 *)
| '*' { TIMES } | '/' { DIV } | '%' { MOD } | "mod" { MOD }
| '+' { PLUS } | '-' { MINUS }
| "==" { EQQ } | "<>" { NEQ }
| '<' { LT } | '>' { GT } | "<=" { LE } | ">=" { GE }
| '&' { AND } | "&&" { AND } | '|' { OR } | "||" { OR }

    (* 制御文のための予約語 *)
| "if" { IF } | "then" { THEN } | "else" { ELSE } | "end-if" { ENDIF }
| "while" { WHILE } | "do" { DO } | "end-while" { ENDWHILE }
| "for" { FOR } | "to" { TO } | "end-for" { ENDFOR }

    (* printとshow_status: 出力命令 *)
| "print" { PRINT }
| "show_status" { DUMP }

    (* 予約語にマッチしない語は、識別子 *)
| ['a'-'z' 'A'-'Z' '$']['a'-'z' 'A'-'Z' '$' '0'-'9''_''*']*
    { IDENT (Lexing.lexeme lexbuf) }
