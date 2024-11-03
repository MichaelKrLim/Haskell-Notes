find_value :: Int -> [Int] -> Bool
find_value value [] = False
find_value value (first:remainder) = if value == first then True else find_value value remainder

reverse_list :: [Int] -> [Int]
reverse_list [] = []
reverse_list (element:remainder) = reverse_list remainder ++ [element]

find_value_index :: Int -> [Int] -> Int
find_value_index value list = find_value_index_helper 0 value list

find_value_index_helper :: Int -> Int -> [Int] -> Int
find_value_index_helper _ _ [] = -1
find_value_index_helper index value (first:remainder) = if value == first then index else find_value_index_helper (index+1) value remainder

remove_element :: Int -> Int -> [Int] -> [Int]
remove_element _ _ [] = []
remove_element current_index index (element:remainder) = if current_index == index then remainder else (element:remove_element (current_index+1) index remainder)

smallest_element :: [Int] -> Int
smallest_element list = smallest_element_helper (maxBound::Int) list

smallest_element_helper :: Int -> [Int] -> Int
smallest_element_helper smallest [] = smallest
smallest_element_helper smallest (element:remainder) = smallest_element_helper (if smallest >= element then element else smallest) remainder

sort_list :: [Int] -> [Int]
sort_list [] = []
sort_list list = smallest:sort_list remainder_list

	where
		smallest = smallest_element list
		smallest_index = find_value_index smallest list
		remainder_list = remove_element 0 smallest_index list

square :: Int -> Int
square n = n*n

square_list :: [Int] -> [Int]
square_list [] = []
square_list (element:remainder) = square element : square_list remainder

better_map :: (Int -> Int) -> [Int] -> [Int]
better_map _ [] = []
better_map func (element:remainder) = func element : better_map func remainder

sum_list :: [Int] -> Int
sum_list [] = 0
sum_list (element:remainder) = element + sum_list remainder

product_list :: [Int] -> Int
product_list [] = 1
product_list (element:remainder) = element * product_list remainder

func :: Int -> (Int -> Int -> Int) -> [Int] -> Int
func initial _ [] = initial
func initial transform (element:remainder) = transform element (func initial transform remainder)
