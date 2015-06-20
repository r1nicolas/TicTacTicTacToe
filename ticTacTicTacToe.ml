type player = PX | PO | None

type square = ticTacToe.tab | O | X

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
	| 1 -> (ticTacToe.play p n2 s11, s12, s13, s21, s22, s23, s31, s32, s33)
	| 2 -> (s11, ticTacToe.play p n2 s12, s13, s21, s22, s23, s31, s32, s33)
	| 3 -> (s11, s12, ticTacToe.play p n2 s13, s21, s22, s23, s31, s32, s33)
	| 4 -> (s11, s12, s13, ticTacToe.play p n2 s21, s22, s23, s31, s32, s33)
	| 5 -> (s11, s12, s13, s21, ticTacToe.play p n2 s22, s23, s31, s32, s33)
	| 6 -> (s11, s12, s13, s21, s22, ticTacToe.play p n2 s23, s31, s32, s33)
	| 7 -> (s11, s12, s13, s21, s22, s23, ticTacToe.play p n2 s31, s32, s33)
	| 8 -> (s11, s12, s13, s21, s22, s23, s31, ticTacToe.play p n2 s32, s33)
	| 9 -> (s11, s12, s13, s21, s22, s23, s31, s32, ticTacToe.play p n2 s33)
	| _ -> invalid_argument "tab only have 9 square"

let squareIsTable s = match s with
	| O | X -> false
	| _ -> true

let playable n1 n2 (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n1 with
	| 1 -> if ((squareIsTable s11) && (ticTacToe.playable n2 s11)) then true else false
	| 2 -> if ((squareIsTable s12) && (ticTacToe.playable n2 s12) then true else false
	| 3 -> if ((squareIsTable s13) && (ticTacToe.playable n2 s13) then true else false
	| 4 -> if ((squareIsTable s21) && (ticTacToe.playable n2 s21) then true else false
	| 5 -> if ((squareIsTable s22) && (ticTacToe.playable n2 s22) then true else false
	| 6 -> if ((squareIsTable s23) && (ticTacToe.playable n2 s23) then true else false
	| 7 -> if ((squareIsTable s31) && (ticTacToe.playable n2 s31) then true else false
	| 8 -> if ((squareIsTable s32) && (ticTacToe.playable n2 s32) then true else false
	| 9 -> if ((squareIsTable s33) && (ticTacToe.playable n2 s33) then true else false
	| _ -> false

