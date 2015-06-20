type square = X | O | None

type tab = square * square * square * square * square * square * square * square * square

let victory t = match t with
	| (O, O, O, _, _, _, _, _, _)
	| (_, _, _, O, O, O, _, _, _)
	| (_, _, _, _, _, _, O, O, O)
	| (O, _, _, _, O, _, _, _, O)
	| (_, _, O, _, O, _, O, _, _)
	| (O, _, _, O, _, _, O, _, _)
	| (_, O, _, _, O, _, _, O, _)
	| (_, _, O, _, _, O, _, _, O) -> O
	| (X, X, X, _, _, _, _, _, _)
	| (_, _, _, X, X, X, _, _, _)
	| (_, _, _, _, _, _, X, X, X)
	| (X, _, _, _, X, _, _, _, X)
	| (_, _, X, _, X, _, X, _, _)
	| (X, _, _, X, _, _, X, _, _)
	| (_, X, _, _, X, _, _, X, _)
	| (_, _, X, _, _, X, _, _, X) -> X
	| _ -> None

let play p n (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n with
	| 1 -> (p, s12, s13, s21, s22, s23, s31, s32, s33)
	| 2 -> (s11, p, s13, s21, s22, s23, s31, s32, s33)
	| 3 -> (s11, s12, p, s21, s22, s23, s31, s32, s33)
	| 4 -> (s11, s12, s13, p, s22, s23, s31, s32, s33)
	| 5 -> (s11, s12, s13, s21, p, s23, s31, s32, s33)
	| 6 -> (s11, s12, s13, s21, s22, p, s31, s32, s33)
	| 7 -> (s11, s12, s13, s21, s22, s23, p, s32, s33)
	| 8 -> (s11, s12, s13, s21, s22, s23, s31, p, s33)
	| 9 -> (s11, s12, s13, s21, s22, s23, s31, s32, p)
	| _ -> invalid_argument "tab only have 9 square"

let playable n (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n with
	| 1 -> if (s11 == None) then true else false
	| 2 -> if (s12 == None) then true else false
	| 3 -> if (s13 == None) then true else false
	| 4 -> if (s21 == None) then true else false
	| 5 -> if (s22 == None) then true else false
	| 6 -> if (s23 == None) then true else false
	| 7 -> if (s31 == None) then true else false
	| 8 -> if (s32 == None) then true else false
	| 9 -> if (s33 == None) then true else false
	| _ -> false

let squareToString s = match s with
	| X -> "X"
	| O -> "o"
	| None -> "-"

let line1ToString (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	(squareToString s11) ^ " " ^ (squareToString s12) ^ " " ^ (squareToString s13)

let line2ToString (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	(squareToString s21) ^ " " ^ (squareToString s22) ^ " " ^ (squareToString s23)

let line3ToString (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	(squareToString s31) ^ " " ^ (squareToString s32) ^ " " ^ (squareToString s33)
