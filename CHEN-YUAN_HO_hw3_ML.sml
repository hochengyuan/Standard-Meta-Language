(* 1. avg_list *)
(* average of list = sum of list / length of list *)
fun avg_list [] = raise Empty |
    avg_list xs = 
        let
            val (sum, length) =
      	        foldl (fn (x, (subtotal,len)) 
      	        (* use real number to set the input list as real list  *)
      	        => (x + subtotal, len + 1.0))(0.0, 0.0) xs
        in
      	(sum / length)
   end;
 
(* 2. get_index *)
(* initially, set NoItem when exception occurs. *)
exception NoItem;
fun get_index inputList index = 
    if index < 0 then 
        raise NoItem
	else
	(* run case switching to define cases in list length of empty or non-empty*)
	   	case inputList of
			[] => raise NoItem | 
			(* run tail recursion to index *)
			(* therefore, we can solve the given index out of list *)
        	(x::inputList) => if index = 0 then x 
                              else get_index inputList (index - 1);

(* 3. get_odd_midpoint *)
(* the midpoint in odd int list is located in the index of (N/2 + 1) if length is N*)
fun get_odd_midpoint odd_List =
    let
        (* calculate the size of length *)
        fun length [] = 0 |
            length (head :: rest) = 1 + length rest
    in
        (* notice: the list is 0-based, do not minus 1 after division *)
        get_index odd_List ((length odd_List) div 2) 
    end ;

(* 4. get_even_midpoint *)
(* The methodology is similar but different with get_odd_midpoint. Instead, we should not adopt function 'get_index' because the input is real list, and thus we need to define new function 'get_real_index' for real list*)
fun get_real_index inputList index = 
    if index < 0 then 
    raise NoItem
	       else
	          	case inputList of
			    	[] => raise NoItem | 
            	    (x::inputList) => 
            	                     (* force the input list to be real list *) 
            	                     if index= 0 then x + 0.0 
                                     else get_index inputList (index - 1);

fun get_even_midpoint even_List =
    let
        fun length [] = 0 |
            length (head :: rest) = 1 + length rest
    in
        (* get the average between the middle two items *)
        (
          (get_real_index even_List ((length even_List) div 2)) + 
          (get_real_index even_List (((length even_List) div 2)-1))
         ) / 2.0
    end ;

(* 5. get_median *)
(* We need to sort the input list before calculating the median *)

(* Function issorted checks if an real list is sorted *)
fun issorted [] = true  |
        issorted [x] = true |
        (* let input list as real list 'by + 0.0' *)
        issorted (x::y::t) = (x + 0.0) <= (y + 0.0) andalso issorted(y::t);
 
(* Function bubble for run the bubbling *)
fun bubble [] = [] |
    bubble [x] = [x] |
    bubble (x::y::t) = 
                       (* let input list as real list 'by + 0.0' *)
                       (* bubble: swap if x > y *)
                       if (x > y) then (y + 0.0)::(bubble (x::t))
                       else (x + 0.0)::(bubble (y::t));
 
(* Call bubble on list until it is sorted *)
fun bubbleSort [] = [] |
    bubbleSort l = if (issorted l) then
                        l 
                   else 
                        bubbleSort (bubble l);

(* Also, we need to set function of passing real parameter to find median of odd list *)
fun get_real_odd_midpoint real_odd_list =
    let
        fun length [] = 0 |
            length (head :: rest) = 1 + length rest
    in
        get_real_index real_odd_list ((length real_odd_list) div 2) 
    end ;

fun get_median given_list =
    let
        fun length [] = 0 |
            length (head :: rest) = 1 + length rest
    in
        (* even list length mod 2 = 0, whereas even list length mod 2 = 1 *)
        (* bubbleSort first then get_even_midpoint *)
        if ((length given_list) mod 2 = 0) then 
            get_even_midpoint (bubbleSort given_list)
        else 
            get_real_odd_midpoint (bubbleSort given_list)
    end;

(* 6. listsum *)
(* in order to check the listsum meet the exact list aggregate, I use tail recursion to keep check if the sum of tail is same as the listsum. If so, it is true, and vice versa*)
(* sum of empty list is 0 *)
fun listsum [] n = if n = 0 then
                        true
                   else
                        false
|   listsum (x::xs) n = listsum xs (n-x);

(* 7. isten *)
(* listsum is bool, and the result of isten is bool, too. In this case, we adopt listsum to check whether sum of list x is ten or not*)
fun isten x = listsum x 10;


(* 8. zip *)
(* To make the concept concrete, we take an example for zip ([1,2,3] , [4,5,6]). The result should be val it = [(1,4),(2,5),(3,6)] : (int * int) list *)
(* Define the exception Mismatch *)
exception Mismatch;
(* In zip, the length of two given list must be same. We do tail recursion to keep matching tails to check whether they are same in length *)
(* If not same in size, raise Mismatch *)
fun zip ([] , []) = [] |
    zip (_ , []) = raise Mismatch |
    zip ([] , _) = raise Mismatch |
    zip (x_head::x_tail , y_head::y_tail) = (x_head , y_head)::(zip (x_tail , y_tail));


(* 9. unzip *)
(* To make the concept concrete, we take an example for unzip [(1,4),(2,5),(3,6)]. The result should be val it = ([1,2,3],[4,5,6]) : int list * int list *)
(* unzip is inverse of function zip *)
fun unzip givenTupleList = 
    (* utilize foldr to do tail recursion in order to mapping unzip tuples *)
    foldr (fn ((a , b) , (a_list , b_list)) 
    => (a :: a_list , b :: b_list)) ([] , []) givenTupleList;

(* 10. scan_left *)
(* val scan_left = fn: (・a -> ・b -> ・a) -> ・a -> ・b list -> ・a list *)
(* F as anonymous function fn *)
fun scan_left F y [] = [y] |
    (* tail recursion to do list append one by one from left to right *)
    scan_left F y (x::xs) = [y] @ (scan_left F (F y x) xs);

(* 11. fact_list *)
(* adopt the function countup from the question stems *)
fun countup n =
let fun countup' 0 l = l
| countup' i l = countup' (i - 1) (i::l)
in
countup' n []
end ;

(* notice: 0! = 1 *)
fun fact_list 0 = [1] |
        (* fact_list inputNumber for the [1,2!,...,inputNumber!] *)
    	fact_list inputNumber =
                (* set fn for (N*N-1) by the tail-recursive nature of scan_left. By definition of factorial list, we start from 1, and let only tail item to as the role y in scan_left to avoid double 1 in the result *)
                scan_left (fn x => fn y => x * y) 1 (tl (countup inputNumber));
