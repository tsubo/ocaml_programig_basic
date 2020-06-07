(* 目的: 鶴と亀の合計数 x と鶴の足の合計 y から鶴の数を求める *)
(* tsurukame : int -> int -> int *)
let tsurukame x y = (x * 4 - y) / 2

(* テスト *)
let test1 = tsurukame 2 6 = 1
let test2 = tsurukame 3 8 = 2
let test2 = tsurukame 3 10 = 1
let test2 = tsurukame 4 12 = 2