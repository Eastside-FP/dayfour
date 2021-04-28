convert _fn [] = []
convert fn (x:xs) = fn x : convert fn xs
