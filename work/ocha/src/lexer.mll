(*****
 * File lexer.mll: �֤���׸���Τ���λ������
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****)


{
open Parser

let line_no = ref 0
} 
rule token = parse
  [' ' '\t' ]         { token lexbuf }	(* ������ɤߤȤФ� *)
| '\n'                { incr line_no; token lexbuf }  (* ����������ɤߤȤФ� *)
| eof                 { EOF }  (* �ե����뽪ü���� *)

| '#' [^ '\n']+       { token lexbuf }  (* # �ʸ�ϥ����ȤȤ����ɤߤȤФ� *)

| "true" { BOOL true } | "false" { BOOL false }  (* ������� *)

| ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) } (* ��������� *)

| '\'' _ '\'' { CHAR ((Lexing.lexeme lexbuf).[1]) }  (* ʸ����� *)

| '"' [^ '"']* '"'    {  (* ʸ������� *)
  let s = Lexing.lexeme lexbuf in
  STRING (String.sub s 1 (String.length s - 2)) }

    (* ���ڤ국�� *)
| ';' { SEMI } | ',' { COMMA } | '[' { BRA } | ']' { ARB }

    (* ��Ӥ����� *)
| '=' { EQ } | ":=" { ASSIGN }

    (* �黻�� *)
| '*' { TIMES } | '/' { DIV } | '%' { MOD } | "mod" { MOD }
| '+' { PLUS } | '-' { MINUS }
| "==" { EQQ } | "<>" { NEQ }
| '<' { LT } | '>' { GT } | "<=" { LE } | ">=" { GE }
| '&' { AND } | "&&" { AND } | '|' { OR } | "||" { OR }

    (* ����ʸ�Τ����ͽ��� *)
| "if" { IF } | "then" { THEN } | "else" { ELSE } | "end-if" { ENDIF }
| "while" { WHILE } | "do" { DO } | "end-while" { ENDWHILE }
| "for" { FOR } | "to" { TO } | "end-for" { ENDFOR }

    (* print��show_status: ����̿�� *)
| "print" { PRINT }
| "show_status" { DUMP }

    (* ͽ���˥ޥå����ʤ���ϡ����̻� *)
| ['a'-'z' 'A'-'Z' '$']['a'-'z' 'A'-'Z' '$' '0'-'9''_''*']*
    { IDENT (Lexing.lexeme lexbuf) }
