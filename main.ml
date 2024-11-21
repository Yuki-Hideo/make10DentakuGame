let remove_whitespace str =
  let is_not_whitespace c = c != ' ' && c != '\t' && c != '\n' && c != '\r' in
  String.of_seq (String.to_seq str |> Seq.filter is_not_whitespace)

let () =
  try
    let start_time = Unix.gettimeofday () in (* 開始時刻を記録 *)
    let count = ref 0 in
    let done_patterns =  [|None; None; None;|] in
    while true do
      print_string "enter formula for making 10 \n(you can use \"!, ?, %, #, /, ^, =\", \nand you have to use 4 characters at least): ";
      let input_string3 = read_line () in
      let input_string2 = remove_whitespace input_string3 in
      let input_string = input_string2 ^ "\n" in
      (* 既に使ったパターンならループ先頭に戻る *)
      if 
        (match done_patterns.(0) with 
         | Some p -> p = input_string 
         | None -> false) ||
        (match done_patterns.(1) with 
         | Some p -> p = input_string 
         | None -> false) ||
        (match done_patterns.(2) with 
         | Some p -> p = input_string 
         | None -> false)
      then begin
        print_string "\n\n!!! you have already entered this formula.\n\n\n";
      end
      else
        begin
          try
            let lexbuf = Lexing.from_string input_string in
            let result = Parser.main Lexer.token lexbuf in
            print_newline ();
            print_string "###############\n# result: ";
            print_int result;
            print_string " #\n###############";
            print_newline ();
            print_newline ();

            (* 式が3文字以下ならエラーメッセージを表示してループ先頭に戻る *)
            if String.length input_string <= 3 then begin
              print_string "\n\n!!! you need to use 4 characters at least.\n\n\n";
              raise Exit
            end;

            (* resultが１０だったら保存、カウントを１増やす *)
            if result == 10 then 
            begin
              print_string "you made 10! nice!\n\n";
              done_patterns.(!count) <- Some input_string;
              count := !count + 1;
            end;
            (* 既につくったパターンを表示 *)
            print_string "★your patterns:\n";
            let () =
              Array.iter (fun x ->
                  match x with
                  | Some s -> 
                      print_string s;
                  | None -> 
                      print_string "None\n"
              ) done_patterns in
            print_newline ();
            (* １０を３回作ったらクリア *)
            if !count == 3 then begin
              print_string "Game clear!!!!!\n";
              let end_time = Unix.gettimeofday () in (* 終了時刻を記録 *)
              let elapsed_time = end_time -. start_time in
              Printf.printf "You completed the game in %.2f seconds.\n" elapsed_time;
              (* print_string "The answer is this.\n\n\n
  | '!'            { TWICEPLUS }
  | '?'            { POWER }
  | '%'            { CUBE }
  | '#'            { REVERSEMINUS }
  | '/'            { REMAINDERFIVE }
  | '^'            { BIGBANG }
  | '='            { BOMB }
  
  
  | expr TWICEPLUS expr     { $1 + 2 * $3 }
  | expr POWER expr         { Float.pow (float $1) (float $3) |> int_of_float }
  | expr CUBE               { $1 * $1 * $1 }
  | expr REVERSEMINUS expr  { - $1 + $3 }
  | expr REMAINDERFIVE expr    { ($1 + $3) mod 5 }
  | expr BIGBANG expr       { $1 * $1 + $3 * $3 }
  | expr BOMB expr          { $1 * 10 + $3 }\n\n"; *)
              exit 0;
            end;
          with
          | Exit -> () (* ループを継続するため何もしない *)
          | Stdlib.Parsing.Parse_error -> 
              print_string "\n### Invalid formula. Try again. ###\n\n";
          | Failure "lexing: empty token" ->
             print_string "\n### Empty input. Try again. ###\n\n"
        end;
    done
  with
  | Lexer.Eof -> print_string "End of input\n"
