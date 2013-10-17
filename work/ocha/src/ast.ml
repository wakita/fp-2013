(*****
 * File ast.ml: 構文木を解釈実行するインタプリタの定義
 * ちなみに AST とは Abstract Syntax Tree = 抽象構文木のこと
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****)

type name = string

exception Fatal
exception Undefined of name
exception Return_value of int
exception No_return_value of name

type value =
  | V_undef
  | V_bool of bool | V_int of int | V_char of char | V_string of string
  | V_array of value array

type location = { name: name; mutable value: value }
type state = location list

open Printf

(* インタプリタ中のデータの値と型を画面に表示するための関数。
   print文での処理を実装するのが本来の目的ですが、インタプリタのデバッグにも便利でした。 *)

let rec fprintf_value p v =
  match v with
  | V_undef -> fprintf p "? : ?"	(* 初期化されていない値を表示 *)
  | V_bool true -> fprintf p "true : bool" (* 論理値を表示 *)
  | V_bool false -> fprintf p "false : bool"
  | V_int i -> fprintf p "%d : int" i	(* 整数値を表示 *)
  | V_char c -> fprintf p "%c : char" c	(* 文字を表示 *)
  | V_string s -> fprintf p "\"%s\" : string" s	(* 文字列を表示 *)
  | V_array vs ->			(* 配列を表示 *)
      match Array.to_list vs with
      |	[] -> fprintf p "[ ] : array"
      |	[ v ] -> fprintf p "[ %a ] : array" fprintf_value v
      |	(v :: vs) ->
	  fprintf p "[ %a" fprintf_value v;
	  List.iter (fun v -> fprintf p ", %a" fprintf_value v) vs;
	  fprintf p " ] : array"

(* 宣言された変数の状態を画面に表示するための関数。
   show_status文の処理を実装している。インタプリタのデバッグにも便利でした。 *)

let print_state s =
  Printf.printf "State:\t[";
  (match List.rev s with
  | [] -> ()
  | [ loc ] -> Printf.printf " %s = %a" loc.name fprintf_value loc.value
  | loc :: locs ->
      Printf.printf " %s = %a" loc.name fprintf_value loc.value;
      List.iter (fun loc ->
	print_string ";\n";
	Printf.printf "\t  %s = %a" loc.name fprintf_value loc.value)
	locs);
  print_endline " ]";
  print_newline ()

let lookup name s =
  List.find (fun loc -> loc.name = name) s

type expr =
  | Bool of bool | Int of int | Char of char | String of string
  | Ident of name
  | Unary of string * expr
  | Binary of string * expr * expr
  | Array of expr list | A_get of name * expr

exception Unary_type_mismatch of name * value
exception Binary_type_mismatch of name * value * value
exception Array_out_of_border of value * value
exception Array_type_mismatch of name * value * value

(* 演算子の働きを記述したデータベース。演算子は単項(unary)演算子と二項
    (binary)演算子に分類し別々のデータベースに記述している *)

type unary = value -> value
let unaries = ref []
let lookup_unary name =
  try List.assoc name !unaries
  with Not_found -> raise (Undefined name)
let new_unary name f =
  unaries := (name, f) :: !unaries

type binary = value -> value -> value
let binaries = ref []
let lookup_binary name =
  try List.assoc name !binaries
  with Not_found -> raise (Undefined name)
let new_binary name f =
  binaries := (name, f) :: !binaries

(* 「お茶」言語がサポートする各種演算子に関するデータベースを構築して
   いる。それぞれの記号が、演算子としてどのような意味を持つのかを記述
   してある。一部の演算子は引数の型に関して多重定義されているために、
   定義がやや複雑になっている。*)

let _ =
  new_unary "-" (fun v ->
    match v with V_int i -> V_int (0 - i)
    | _ -> raise (Unary_type_mismatch ("-", v)));

  new_binary "=" (fun v1 v2 -> V_bool (v1 = v2));

  new_binary "<>" (fun v1 v2 -> V_bool (v1 <> v2));

  new_binary ">" (fun v1 v2 ->
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_bool (i1 > i2)
    | (V_char c1, V_char c2) -> V_bool (c1 > c2)
    | (V_string s1, V_string s2) -> V_bool (s1 > s2)
    | _ -> raise (Binary_type_mismatch (">", v1, v2)));

  new_binary "<" (fun v1 v2 ->
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_bool (i1 < i2)
    | (V_char c1, V_char c2) -> V_bool (c1 < c2)
    | (V_string s1, V_string s2) -> V_bool (s1 < s2)
    | _ -> raise (Binary_type_mismatch ("<", v1, v2)));

  new_binary ">=" (fun v1 v2 ->
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_bool (i1 >= i2)
    | (V_char c1, V_char c2) -> V_bool (c1 >= c2)
    | (V_string s1, V_string s2) -> V_bool (s1 >= s2)
    | _ -> raise (Binary_type_mismatch (">=", v1, v2)));

  new_binary "<=" (fun v1 v2 ->
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_bool (i1 <= i2)
    | (V_char c1, V_char c2) -> V_bool (c1 <= c2)
    | (V_string s1, V_string s2) -> V_bool (s1 <= s2)
    | _ -> raise (Binary_type_mismatch ("<=", v1, v2)));

  let binary_int name f v1 v2 =
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_int (f i1 i2)
    | _ -> raise (Binary_type_mismatch (name, v1, v2)) in
  
  new_binary "+" (fun v1 v2 ->
    match (v1, v2) with
    | (V_int i1, V_int i2) -> V_int (i1 + i2)
    | (V_string s1, V_string s2) -> V_string (s1 ^ s2)
    | _ -> raise (Binary_type_mismatch ("+", v1, v2)));
  
  new_binary "-" (binary_int "-" (-));

  new_binary "*" (binary_int "*" ( * ));

  new_binary "/" (binary_int "/" (/));

  new_binary "%" (binary_int "mod" (mod))

(* eval [expression] [status]: expressionで表された式を評価する処理を
   実装する関数。status はこの時点での変数への値の代入状況を表現する。

   この関数は、評価結果の値と評価後の変数への代入状況をペアとして返す。
   *)

let rec eval expr s =
  try
    match expr with
    | Bool b -> (V_bool b, s)
    | Int n -> (V_int n, s)
    | Char c -> (V_char c, s)
    | String str -> (V_string str, s)
    | Ident name ->
	(try ((lookup name s).value, s)
	with Not_found ->	raise (Undefined name))
    | Unary (name, e) ->
	let (v, s) = eval e s in
	((lookup_unary name) v, s)
    | Binary (name, e1, e2) ->
	let (v1, s) = eval e1 s in
	let (v2, s) = eval e2 s in
	((lookup_binary name) v1 v2, s)
    | Array es ->
	let (vs, s) =
	  List.fold_left
	    (fun (vs, s) e ->
	      let v, s = eval e s in
	      (v :: vs, s))
	    ([], s) es in
	(V_array (Array.of_list (List.rev vs)), s)
    | A_get (name, e) ->
	let v = (lookup name s).value in
	let (i, s) = eval e s in
	match (v, i) with
	| (V_array v, V_int i) ->
	    if i >= Array.length v then
	      raise (Array_out_of_border (V_array v, V_int i));
	    (v.(i), s)
	| (V_string str, V_int i) ->
	    if i >= String.length str then
	      raise (Array_out_of_border (V_string str, V_int i));
	    (V_char str.[i], s)
	| _ -> raise (Array_type_mismatch (name, v, i))
  with Undefined name ->
    fprintf stderr "\n[%s] は未定義です。きちんと定義してから使って下さい。\n" name;
    raise Exit

type command =
  | Define of name * expr  (* year = 2004; や years = [ 2000, 2001, 2002 ]; *)
  | Set of name * expr     (* year := year + 1; *)
  | A_set of name * expr * expr  (* years[1] := 1998; *)
  | Seq of command list          (* tmp := a; a := b; b := tmp; *)
  | If of expr * command * command  (* if 文 *)
  | Skip                            (* 何もしない。else 節が省略されたときに用いる *)
  | While of expr * command         (* while 文 *)
  | For of name * expr * expr * command  (* for 文 *)
  | Print of expr | Dump                 (* 出力文 *)

(* handle [error]: errorの種類に応じて、適切なエラーメッセージを出力し、
   さらにエラー処理を継続する。*)

let handle exn =
  prerr_string "\n";
  match exn with
  | Undefined name ->
      fprintf stderr "\n[%s] という名前は定義されていません。" name;
      prerr_string "この名前をどのように解釈すればよいか分かりません。";
      raise Fatal

  | Unary_type_mismatch (op, v) ->
      fprintf stderr
	"[%s 引数] という演算の実行中にエラーが発生しました。\n" op;
      prerr_string "原因はこの演算では引数のデータをできないからです。";
      prerr_string "引数を計算した値は以下の通りです。\n";
      fprintf stderr "\t引数を計算した値: %a\n" fprintf_value v;
      prerr_string
	"データの種類(: の右側)に注目して、プログラムを訂正して下さい。";
      raise Fatal

  | Binary_type_mismatch (op, v1, v2) ->
      fprintf stderr
	"[引数1 %s 引数2] という演算の実行中にエラーが発生しました。\n" op;
      prerr_string "原因は、この演算では引数のデータを処理できないからです。";
      prerr_string "二つの引数を計算した値は以下の通りです。\n";
      fprintf stderr "\t引数1を計算した値: %a\n" fprintf_value v1;
      fprintf stderr "\t引数2を計算した値: %a\n" fprintf_value v2;
      prerr_string
	"データの種類(: の右側)に注目して、プログラムを訂正して下さい。";
      raise Fatal

  | Array_out_of_border (v, i) ->
      (match (v, i) with
      |	(V_array v, V_int i) ->
	  (* let len = Array.length v in *)
	  prerr_string
	    "配列の要素をアクセスしようとしてエラーが発生しました。\n";
	  prerr_string
	    "原因は配列の大きさの範囲を越えた部分をアクセスしたからです。"
      |	(V_string v, V_int i) ->
	  prerr_string
	    "文字列の要素をアクセスしようとしてエラーが発生しました。\n";
	  prerr_string
	    "原因は文字列の大きさの範囲を越えた部分をアクセスしたからです。"
      |	_ -> ());
      raise Fatal

  | Array_type_mismatch (name, v, i) ->
      fprintf stderr "配列へのアクセスでエラーが生じました。\n";
      (match (v, i) with
      |	(_, V_int _) ->
	  prerr_string
	    "原因は、配列以外のデータを配列としてアクセスしたからです。"
      |	(V_array _, _) ->
	  prerr_string
	    "原因は、配列の添字に整数以外のデータを使ったからです。"
      |	_ -> ());
      raise Fatal

  | exn -> raise exn

(* exec [command] [status]: commandで表された文の実行処理を実装する関
   数。status はこの時点での変数への値の代入状況を表現する。

   この関数は、commandを実行したあとの変数への代入状況を返す。 *)

let rec exec c s =
  try
    match c with
    | Define (name, expr) ->		(* 変数の定義 *)
	let (v, s) = eval expr s in
	{ name = name; value = v } :: s
    | Set (name, a) ->			(* 変数への代入 *)
	(try
	  let (v, s) = eval a s in (lookup name s).value <- v; s
	with Not_found ->
	  { name = name; value = V_undef } :: s)
    | A_set (name, e1, e2) ->		(* 配列への代入 *)
	let v = (lookup name s).value in
	let (i, s) = eval e1 s in
	let (x, s) = eval e2 s in
	(match (v, i) with
	| (V_array v, V_int i) -> v.(i) <- x
	| (V_string str, V_int i) ->
	    (match x with
	    | V_char c -> str.[i] <- c
	    | _ -> raise Fatal)
	| _ -> raise (Array_type_mismatch (name, v, i)));
	s
    | Seq [] -> s			(* 複数の文の順次実行 *)
    | Seq (c :: cs) -> let s' = exec c s in exec (Seq cs) s'
    | Print expr ->
	let (v, s) = eval expr s in
	Printf.printf "\t%a.\n\n%t" fprintf_value v flush;
	s
    | If (e, c1, c2) ->			(* if 文 *)
	let (v, s) = eval e s in
	(match v with
	| V_bool false -> exec c2 s
	| _ -> exec c1 s)
    | While (e, c) ->			(* while 文 *)
	let (v, s) = eval e s in
	(match v with
	| V_bool false -> s
	| _ -> exec (Seq [ c; While (e, c) ]) s)
    | For (name, e1, e2, c) ->		(* for 文 *)
	(let (start, s) = eval e1 s in
	let loc = { name = name; value = start } in
	let s = loc :: s in
	let (stop, s) = eval e2 s in
	match (start, stop) with
	| (V_int start, V_int stop) ->
	    for i = start to stop do
	      loc.value <- V_int i;
	      ignore (exec c s);
	    done
	| _ -> raise Fatal);
	s
    | Skip -> s				(* Skip 文はなにもない *)
    | Dump -> print_state s; flush stdout; s (* インタプリタの状態を表示 *)

  with exn -> handle exn		(* 例外が発生したら例外処理をする *)
