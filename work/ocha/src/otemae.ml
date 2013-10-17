(*****
 * File imp.ml: 「お茶」言語の主処理部
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****)

open Printf
open Ast

let failure_exit () =
  prerr_string "\nこれ以上、実行を続けられないので、実行を中断します。";
  prerr_string "プログラムを直して完成してください。\n";
  flush stdout; flush stderr;
  exit 1

let _ =
  let inport =
    if Array.length Sys.argv >= 2 then open_in Sys.argv.(1)
    else
      (print_string "「お茶」言語インタプリタ「お点前」バージョン0.1\n\n";
       flush stdout;
       stdin) in

  let lexbuf = Lexing.from_channel inport in
  try
    let _ = exec (Parser.otemae Lexer.token lexbuf) [] in
    close_in inport
  with
    Exit ->
      close_in inport;
      failure_exit ()
  | Parsing.Parse_error ->
      close_in inport;
      prerr_string
	("\n以下のトークンの近辺にプログラムの書き誤りがあります：[" ^
	 (Lexing.lexeme lexbuf) ^ "]\n");
      failure_exit ()
  | Fatal ->
      close_in inport;
      failure_exit ()
