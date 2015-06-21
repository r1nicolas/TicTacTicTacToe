type endOfGame = VX | VO | Draw | None

type square = Tab of TicTacToe.tab | O | X

type tab = square * square * square * square * square * square * square * square * square

let squarel1ToString s = match s with
	| O -> "/ - \\"
	| X -> "\\   /"
	| Tab(t) -> TicTacToe.line1ToString t

let squarel2ToString s = match s with
	| O -> "|   |"
	| X -> "  X  "
	| Tab(t) -> TicTacToe.line2ToString t

let squarel3ToString s = match s with
	| O -> "\\ - /"
	| X -> "/   \\"
	| Tab(t) -> TicTacToe.line3ToString t

let tabToString (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	(squarel1ToString s11) ^ " | " ^ (squarel1ToString s12) ^ " | " ^ (squarel1ToString s13) ^ "\n" ^
	(squarel2ToString s11) ^ " | " ^ (squarel2ToString s12) ^ " | " ^ (squarel2ToString s13) ^ "\n" ^
	(squarel3ToString s11) ^ " | " ^ (squarel3ToString s12) ^ " | " ^ (squarel3ToString s13) ^ "\n" ^
	"---------------------\n" ^
	(squarel1ToString s21) ^ " | " ^ (squarel1ToString s22) ^ " | " ^ (squarel1ToString s23) ^ "\n" ^
	(squarel2ToString s21) ^ " | " ^ (squarel2ToString s22) ^ " | " ^ (squarel2ToString s23) ^ "\n" ^
	(squarel3ToString s21) ^ " | " ^ (squarel3ToString s22) ^ " | " ^ (squarel3ToString s23) ^ "\n" ^
	"---------------------\n" ^
	(squarel1ToString s31) ^ " | " ^ (squarel1ToString s32) ^ " | " ^ (squarel1ToString s33) ^ "\n" ^
	(squarel2ToString s31) ^ " | " ^ (squarel2ToString s32) ^ " | " ^ (squarel2ToString s33) ^ "\n" ^
	(squarel3ToString s31) ^ " | " ^ (squarel3ToString s32) ^ " | " ^ (squarel3ToString s33) ^ "\n"


let endOfGame t = match t with
	| (O, O, O, _, _, _, _, _, _)
	| (_, _, _, O, O, O, _, _, _)
	| (_, _, _, _, _, _, O, O, O)
	| (O, _, _, _, O, _, _, _, O)
	| (_, _, O, _, O, _, O, _, _)
	| (O, _, _, O, _, _, O, _, _)
	| (_, O, _, _, O, _, _, O, _)
	| (_, _, O, _, _, O, _, _, O) -> VO
	| (X, X, X, _, _, _, _, _, _)
	| (_, _, _, X, X, X, _, _, _)
	| (_, _, _, _, _, _, X, X, X)
	| (X, _, _, _, X, _, _, _, X)
	| (_, _, X, _, X, _, X, _, _)
	| (X, _, _, X, _, _, X, _, _)
	| (_, X, _, _, X, _, _, X, _)
	| (_, _, X, _, _, X, _, _, X) -> VX
	| (Tab(x), _, _, _, _, _, _, _, _)
	| (_, Tab(x), _, _, _, _, _, _, _)
	| (_, _, Tab(x), _, _, _, _, _, _)
	| (_, _, _, Tab(x), _, _, _, _, _)
	| (_, _, _, _, Tab(x), _, _, _, _)
	| (_, _, _, _, _, Tab(x), _, _, _)
	| (_, _, _, _, _, _, Tab(x), _, _)
	| (_, _, _, _, _, _, _, Tab(x), _)
	| (_, _, _, _, _, _, _, _, Tab(x)) -> None
	| _ -> Draw

let endToString e = match e with
	| VX -> "X wins the game!"
	| VO -> "O wins the game!"
	| Draw -> "Draw!"
	| None -> "U wot m8?"

let play p n1 n2 t : tab = match (n1, t) with
	| (1, (Tab(s11), s12, s13, s21, s22, s23, s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s11 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (Tab(s), s12, s13, s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (O, s12, s13, s21, s22, s23, s31, s32, s33)
			else (X, s12, s13, s21, s22, s23, s31, s32, s33)
	| (2, (s11, Tab(s12), s13, s21, s22, s23, s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s12 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, Tab(s), s13, s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, O, s13, s21, s22, s23, s31, s32, s33)
			else (s11, X, s13, s21, s22, s23, s31, s32, s33)
	| (3, (s11, s12, Tab(s13), s21, s22, s23, s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s13 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, Tab(s), s21, s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, s12, O, s21, s22, s23, s31, s32, s33)
			else (s11, s12, X, s21, s22, s23, s31, s32, s33)
	| (4, (s11, s12, s13, Tab(s21), s22, s23, s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s21 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, Tab(s), s22, s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, s12, s13, O, s22, s23, s31, s32, s33)
			else (s11, s12, s13, X, s22, s23, s31, s32, s33)
	| (5, (s11, s12, s13, s21, Tab(s22), s23, s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s22 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, s21, Tab(s), s23, s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, s12, s13, s21, O, s23, s31, s32, s33)
			else (s11, s12, s13, s21, X, s23, s31, s32, s33)
	| (6, (s11, s12, s13, s21, s22, Tab(s23), s31, s32, s33)) ->
		let s = TicTacToe.play p n2 s23 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, s21, s22, Tab(s), s31, s32, s33)
		else if (p = TicTacToe.O)
			then (s11, s12, s13, s21, s22, O, s31, s32, s33)
			else (s11, s12, s13, s21, s22, X, s31, s32, s33)
	| (7, (s11, s12, s13, s21, s22, s23, Tab(s31), s32, s33)) ->
		let s = TicTacToe.play p n2 s31 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, s21, s22, s23, Tab(s), s32, s33)
		else if (p = TicTacToe.O)
			then (s11, s12, s13, s21, s22, s23, O, s32, s33)
			else (s11, s12, s13, s21, s22, s23, X, s32, s33)
	| (8, (s11, s12, s13, s21, s22, s23, s31, Tab(s32), s33)) ->
		let s = TicTacToe.play p n2 s32 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, s21, s22, s23, s31, Tab(s), s33)
		else if (p = TicTacToe.O)
			then (s11, s12, s13, s21, s22, s23, s31, O, s33)
			else (s11, s12, s13, s21, s22, s23, s31, X, s33)
	| (9, (s11, s12, s13, s21, s22, s23, s31, s32, Tab(s33))) ->
		let s = TicTacToe.play p n2 s33 in
		if (((TicTacToe.victory s) == TicTacToe.None) && (not (TicTacToe.draw s)))
			then (s11, s12, s13, s21, s22, s23, s31, s32, Tab(s))
		else if (p = TicTacToe.O)
			then (s11, s12, s13, s21, s22, s23, s31, s32, O)
			else (s11, s12, s13, s21, s22, s23, s31, s32, X)
	| _ -> invalid_arg "invalid tab"

let playable n1 n2 t = match (n1, t) with
	| (1, (Tab(s11), s12, s13, s21, s22, s23, s31, s32, s33)) -> TicTacToe.playable n2 s11
	| (2, (s11, Tab(s12), s13, s21, s22, s23, s31, s32, s33)) -> TicTacToe.playable n2 s12
	| (3, (s11, s12, Tab(s13), s21, s22, s23, s31, s32, s33)) -> TicTacToe.playable n2 s13
	| (4, (s11, s12, s13, Tab(s21), s22, s23, s31, s32, s33)) -> TicTacToe.playable n2 s21
	| (5, (s11, s12, s13, s21, Tab(s22), s23, s31, s32, s33)) -> TicTacToe.playable n2 s22
	| (6, (s11, s12, s13, s21, s22, Tab(s23), s31, s32, s33)) -> TicTacToe.playable n2 s23
	| (7, (s11, s12, s13, s21, s22, s23, Tab(s31), s32, s33)) -> TicTacToe.playable n2 s31
	| (8, (s11, s12, s13, s21, s22, s23, s31, Tab(s32), s33)) -> TicTacToe.playable n2 s32
	| (9, (s11, s12, s13, s21, s22, s23, s31, s32, Tab(s33))) -> TicTacToe.playable n2 s33
	| _ -> false

let game p t =
	let rec game_tail s p t =
		if ((String.length s) > 3 || (String.length s) < 3 || s.[1] != ' ' || s.[0] < '1' || s.[0] > '9' || s.[2] < '1' || s.[2] > '9')
				then begin
					print_endline "Incorrect format.";
					game_tail (read_line ()) p t
				end
		else if (playable ((int_of_char s.[0]) - (int_of_char '0')) ((int_of_char s.[2]) - (int_of_char '0')) t)
			then begin
				let t = play p ((int_of_char s.[0]) - (int_of_char '0')) ((int_of_char s.[2]) - (int_of_char '0')) t in
				print_string (tabToString t);
				match (endOfGame t) with
					| VO -> VO
					| VX -> VX
					| Draw -> Draw
					| None ->
						print_endline ((TicTacToe.squareToString (TicTacToe.other p)) ^ "'s turn to play");
						game_tail (read_line ()) (TicTacToe.other p) t
			end
			else begin
				print_endline "Illegal move.";
				game_tail (read_line ()) p t
			end
	in
	print_string (tabToString t);
	print_endline ((TicTacToe.squareToString p) ^ "'s turn to play");
	game_tail (read_line ()) p t

let empty_tab = (Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None),
				 Tab(TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None, TicTacToe.None))

let () =
	print_endline (endToString(game TicTacToe.O empty_tab))
