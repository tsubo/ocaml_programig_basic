(* 目的：ローマ字の駅名と駅名リスト(ekimei_t list型)を受け取ったら、その駅の漢字名を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji ekimei_lst = match ekimei_lst with
    [] -> ""
  | ekimei :: rest ->
      if romaji = ekimei.romaji
        then ekimei.kanji
        else romaji_to_kanji romaji rest
 
(* テスト *)
let test1 = romaji_to_kanji "" global_ekimei_list = ""
let test2 = romaji_to_kanji "ikebukuro" [] = ""
let test3 = romaji_to_kanji "ikebukuro" global_ekimei_list = "池袋"
let test4 = romaji_to_kanji "ogikubo" global_ekimei_list = "荻窪"

(* 目的：漢字の駅名２つと駅間リスト(ekikan_t list型)を受け取ったら、その２つの駅の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_lst = match ekikan_lst with
    [] -> infinity
  | ekikan :: rest ->
      if (ekimei1 = ekikan.kiten && ekimei2 = ekikan.shuten) ||
         (ekimei2 = ekikan.kiten && ekimei1 = ekikan.shuten)
      then ekikan.kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest

(* テスト *)
let test11 = get_ekikan_kyori "" "" global_ekikan_list = infinity
let test12 = get_ekikan_kyori "" "和光市" global_ekikan_list = infinity
let test13 = get_ekikan_kyori "営団成増" "" global_ekikan_list = infinity
let test14 = get_ekikan_kyori "営団成増" "荻窪" global_ekikan_list = infinity
let test15 = get_ekikan_kyori "営団成増" "和光市" global_ekikan_list = 2.1
let test16 = get_ekikan_kyori "和光市" "営団成増" global_ekikan_list = 2.1

(*
  目的：ローマ字の駅名２つと駅間リスト(ekikan_t list型)を受け取ったら、その２つの駅の距離を調べ
       直接つながている場合は「A駅からB駅までは〇kmです」という文字列を返し
       直接つながっていない場合は「A駅とB駅はつながっていません」という文字列を返す
*)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji romaji1 romaji2 =
  let kanji1 = romaji_to_kanji romaji1 global_ekimei_list in
  if kanji1 = "" then romaji1 ^ " という駅は存在しません"
  else let kanji2 = romaji_to_kanji romaji2 global_ekimei_list in
    if kanji2 = "" then romaji2 ^ " という駅は存在しません"
    else let kyori = get_ekikan_kyori kanji1 kanji2 global_ekikan_list in
      if kyori = infinity
      then kanji1 ^ "と" ^ kanji2 ^ "はつながっていません"
      else kanji1 ^ "から" ^ kanji2 ^ "までは " ^ string_of_float kyori ^ " キロです"

(* テスト *) 
let test21 = kyori_wo_hyoji "myougadani" "shinotsuka" 
	    = "myougadani という駅は存在しません" 
let test22 = kyori_wo_hyoji "myogadani" "shinotsuka" 
	    = "茗荷谷から新大塚までは 1.2 キロです" 
let test23 = kyori_wo_hyoji "myogadani" "ikebukuro" 
	    = "茗荷谷と池袋はつながっていません" 
let test24 = kyori_wo_hyoji "tokyo" "ootemachi" 
	    = "ootemachi という駅は存在しません" 
let test25 = kyori_wo_hyoji "tokyo" "otemachi" 
	    = "東京から大手町までは 0.6 キロです" 
