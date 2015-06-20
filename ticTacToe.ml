type square = X | O | None

type tab = square * square * square * square * square * square * square * square * square

let victory (s11, s12, s13, s21, s22, s23, s31, s32, s33) =
	if (s11 == s12 && s11 == s13 && s11 != None)
		then s11
	else if (s21 == s22 && s21 == s23 && s21 != None)
		then s21
	else if (s31 == s32 && s31 == s33 && s31 != None)
		then s31
	else if (s11 == s21 && S11 == s31 && s11 != None)
		then s11
	else if (s12 == s22 && S12 == s32 && s12 != None)
		then s12
	else if (s13 == s23 && S13 == s33 && s13 != None)
		then s13
	else if (s11 == s22 && s11 == s33 && s11 != None)
		then s11
	else if (s13 == s22 && s11 == s31 && s13 != None)
		then s13
		else None

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

let playable p n (s11, s12, s13, s21, s22, s23, s31, s32, s33) = match n with
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

