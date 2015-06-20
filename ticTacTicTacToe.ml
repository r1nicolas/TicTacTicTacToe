type player = PX | PO | None

type square = TicTacToe.tab | O | X

type tab = square * square * square * square * square * square * square * square * square

let victory t = match t with
	| (O, O, O, _, _, _, _, _, _)
	| (_, _, _, O, O, O, _, _, _)
	| (_, _, _, _, _, _, O, O, O)
	| (O, _, _, _, O, _, _, _, O)
	| (_, _, O, _, O, _, O, _, _)
	| (O, _, _, O, _, _, O, _, _)
	| (_, O, _, _, O, _, _, O, _)
	| (_, _, O, _, _, O, _, _, O) -> PO
	| (X, X, X, _, _, _, _, _, _)
	| (_, _, _, X, X, X, _, _, _)
	| (_, _, _, _, _, _, X, X, X)
	| (X, _, _, _, X, _, _, _, X)
	| (_, _, X, _, X, _, X, _, _)
	| (X, _, _, X, _, _, X, _, _)
	| (_, X, _, _, X, _, _, X, _)
	| (_, _, X, _, _, X, _, _, X) -> PX
	| _ -> None

let play p n1 n2 (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n1 with
	| 1 -> 
		if (TicTacToe.victory TicTacToe.play p n2 s11 == TicTacToe.None)
			then (TicTacToe.play p n2 s11, s12, s13, s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (O, s12, s13, s21, s22, s23, s31, s32, s33)
			else (X, s12, s13, s21, s22, s23, s31, s32, s33)
	| 2 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, TicTacToe.play p n2 s12, s13, s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, O, s13, s21, s22, s23, s31, s32, s33)
			else (s11, X, s13, s21, s22, s23, s31, s32, s33)
	| 3 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, TicTacToe.play p n2 s13, s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, O, s21, s22, s23, s31, s32, s33)
			else (S11, s12, X, s21, s22, s23, s31, s32, s33)
	| 4 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, TicTacToe.play p n2 s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, O, s22, s23, s31, s32, s33)
			else (S11, s12, s13, X, s22, s23, s31, s32, s33)
	| 5 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, s21, TicTacToe.play p n2 s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, s21, O, s23, s31, s32, s33)
			else (S11, s12, s13, s21, X, s23, s31, s32, s33)
	| 6 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, s21, s22, TicTacToe.play p n2 s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, s21, s22, O, s31, s32, s33)
			else (S11, s12, s13, s21, s22, X, s31, s32, s33)
	| 7 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, s21, s22, s23, TicTacToe.play p n2 s31, s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, s21, s22, s23, O, s32, s33)
			else (S11, s12, s13, s21, s22, s23, X, s32, s33)
	| 8 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, s21, s22, s23, s31, TicTacToe.play p n2 s32, s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, s21, s22, s23, s31, O, s33)
			else (S11, s12, s13, s21, s22, s23, s31, X, s33)
	| 9 ->
		if (TicTacToe.victory TicTacToe.play p n2 s12 == TicTacToe.None)
			then (s11, s12, s13, s21, s22, s23, s31, s32, TicTacToe.play p n2 s33)
		else if (p = TicTacToe.O)
			then (S11, s12, s13, s21, s22, s23, s31, s32, O)
			else (S11, s12, s13, s21, s22, s23, s31, s32, X)
	| _ -> invalid_arg "tab only have 9 square"

let squareIsTable s = match s with
	| O | X -> false
	| _ -> true

let playable n1 n2 (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n1 with
	| 1 -> if ((squareIsTable s11) && (TicTacToe.playable n2 s11)) then true else false
	| 2 -> if ((squareIsTable s12) && (TicTacToe.playable n2 s12)) then true else false
	| 3 -> if ((squareIsTable s13) && (TicTacToe.playable n2 s13)) then true else false
	| 4 -> if ((squareIsTable s21) && (TicTacToe.playable n2 s21)) then true else false
	| 5 -> if ((squareIsTable s22) && (TicTacToe.playable n2 s22)) then true else false
	| 6 -> if ((squareIsTable s23) && (TicTacToe.playable n2 s23)) then true else false
	| 7 -> if ((squareIsTable s31) && (TicTacToe.playable n2 s31)) then true else false
	| 8 -> if ((squareIsTable s32) && (TicTacToe.playable n2 s32)) then true else false
	| 9 -> if ((squareIsTable s33) && (TicTacToe.playable n2 s33)) then true else false
	| _ -> false

let squarel1ToString s = match s with
	| O -> "/ - \\"
	| X -> "\\   /"
	| _ -> TicTacToe.line1ToString s

let squarel2ToString s = match s with
	| O -> "|   |"
	| X -> "  X  "
	| _ -> TicTacToe.line2ToString s

let squarel1ToString s = match s with
	| O -> "\\ - /"
	| X -> "/   \\"
	| _ -> TicTacToe.line3ToString s

let tabToString (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	(squarel1ToString s11) ^ " | " ^ (squarel1ToString s12) ^ " | " ^ (squarel1ToString s13) ^ "\n" ^
	(squarel2ToString s11) ^ " | " ^ (squarel2ToString s12) ^ " | " ^ (squarel2ToString s13) ^ "\n" ^
	(squarel3ToString s11) ^ " | " ^ (squarel3ToString s12) ^ " | " ^ (squarel3ToString s13) ^ "\n" ^
	"---------------------" ^
	(squarel1ToString s21) ^ " | " ^ (squarel1ToString s22) ^ " | " ^ (squarel1ToString s23) ^ "\n" ^
	(squarel2ToString s21) ^ " | " ^ (squarel2ToString s22) ^ " | " ^ (squarel2ToString s23) ^ "\n" ^
	(squarel3ToString s21) ^ " | " ^ (squarel3ToString s22) ^ " | " ^ (squarel3ToString s23) ^ "\n" ^
	"---------------------" ^
	(squarel1ToString s31) ^ " | " ^ (squarel1ToString s32) ^ " | " ^ (squarel1ToString s33) ^ "\n" ^
	(squarel2ToString s31) ^ " | " ^ (squarel2ToString s32) ^ " | " ^ (squarel2ToString s33) ^ "\n" ^
	(squarel3ToString s31) ^ " | " ^ (squarel3ToString s32) ^ " | " ^ (squarel3ToString s33) ^ "\n"

let game p t =
	let rec game_tail i p t = match (victory t) with
		| PO -> PO
		| PX -> PX
		| None ->
			print_string (tabToString t);
			print_endline ((TicTacToe.squareToString p) ^ "'s turn to play";
			if ((String.length i) > 3 || (String.length i) < 3 || s.[1] != ' ' || s.[0] < '1' || s.[0] > '9' || s.[2] < '1' || s.[2] > '9')
					then begin
						print_endline "Incorrect format.";
						game_tail (read_line ()) p t
					end
			else if (playable ((int_of_char s.[0]) - (int_of_char '0')) ((int_of_char s.[2]) - (int_of_char '0')) t
				then game_tail (read_line ()) (TicTacToe.other p) (play p ((int_of_char s.[0]) - (int_of_char '0')) ((int_of_char s.[2]) - (int_of_char '0')) t)
				else begin
					print_endline "Illegal move.";
					game_tail (read_line ()) p t
				end
	in
	game_tail (read_line ()) p t

let empty_tab = ((TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 (TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None))

let () =
	game TicTacToe.O empty_tab
