seven :: Int -> Int
seven x = 7

sign :: Int -> Int
sign x 
	| x > 0 = 1
	| x < 0 = -1
	| otherwise =0

signIf :: Int -> Int
signIf x =  
	if x > 0 then 1
		else if x < 0 then -1
			else 0

absP :: Int -> Int
absP x = x * sign(x)

orP :: (Bool, Bool) -> Bool
orP (x,y) =
	if x == False then
		if y == False then False
		else True
	else True

notP :: Bool -> Bool
notP x
	| x == True = False
	| otherwise = True

andP :: (Bool, Bool) -> Bool
andP (x,y) = notP(orP(notP(x),notP(y)))

xorP :: (Bool, Bool) -> Bool
xorP (x,y) = notP(orP(notP(x),notP(y)))

divisible :: (Int, Int) -> Bool
divisible (x,y) = 
    if mod x y == 0 then True
        else False

multiple :: (Int, Int) -> Bool
multiple (x, y) = 
    if mod x y == 0 then True
        else False

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

f1 :: (a) -> (a)
f1 x = let (y,z) = (x,x) in y