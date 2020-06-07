let tsuru_no_ashi = 2
let kame_no_ashi = 4

(* 目的: 鶴の数 x と亀の数 y から、鶴とカメの足の合計本数を求める *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y = x * tsuru_no_ashi + y * kame_no_ashi

(* テスト *)
let test1 = tsurukame_no_ashi 1 1 = 6
let test2 = tsurukame_no_ashi 2 1 = 8
let test3 = tsurukame_no_ashi 1 2 = 10
let test4 = tsurukame_no_ashi 2 2 = 12