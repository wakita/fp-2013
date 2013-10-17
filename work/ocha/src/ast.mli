(*****
 * ast.mli の註釈ファイル
 *
 * Copyright (C) 2004-2013  Ken Wakita
 *****)

type name = string           (* 識別子の内部的な表現は文字列 *)

(* いろいろな型の「値」を区別して表現する *)

type value =
    V_undef  (* 未定義； 初期化されていない変数の値を表現 *)
  | V_bool of bool | V_int of int | V_char of char | V_string of string
  | V_array of value array

(* 変数に格納される値とその集合として表されたインタプリタの状態 *)

type location = { name: name; mutable value: value }
type state = location list

val print_state : state -> unit  (* インタプリタの状態を表示する関数 *)
val lookup : name -> state -> location  (* 変数の状態を返す関数 *)

(* さまざまなエラー *)

exception Fatal              (* 致命的なエラー *)
exception Undefined of name  (* 未定義な変数をアクセスしようとした *)

    (* 演算で不適切な型のデータが使われている *)
exception Unary_type_mismatch of name * value
exception Binary_type_mismatch of name * value * value
    (* 配列へのアクセスに関するエラー *)
exception Array_out_of_border of value * value
exception Array_type_mismatch of name * value * value

(* 構文解析木の式を表す型の宣言 *)

type expr =
    Bool of bool | Int of int | Char of char | String of string
  | Ident of string
  | Unary of string * expr
  | Binary of string * expr * expr
  | Array of expr list | A_get of name * expr

(* 単項演算子と二項演算子の表現 *)

type unary = value -> value
val unaries : (name * unary) list ref
type binary = value -> value -> value
val binaries : (name * binary) list ref

(* 構文解析木の制御構造を表す型の定義 *)

type command =
  | Define of name * expr
  | Set of name * expr
  | A_set of name * expr * expr
  | Seq of command list
  | If of expr * command * command
  | Skip
  | While of expr * command
  | For of name * expr * expr * command
  | Print of expr | Dump

(* インタプリタ本体での処理を表す関数の型定義 *)

val eval : expr -> state -> value * state  (* 式を評価する関数 *)
val exec : command -> state -> state       (* 制御文を実行する関数 *)
