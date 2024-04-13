/**
  Returns the sum 1 + 2 + ... + n
  If n is less than 0, return -1
 **/
pub fn gauss(n: i32) -> i32 {
    let mut sum : i32 = 0;
    if n < 0 {
	return -1;
    }
    for i in 1 ..= n {
	sum = sum + i;
    }
   /* println!("number:{}", sum); */
    return sum




}

/**
  Returns the number of elements in the list that
  are in the range [s,e]
 **/
pub fn in_range(lst: &[i32], s: i32, e: i32) -> i32 {
    let mut num = 0;
    for element in lst.iter() {
	if *element <= e && *element >= s {
	    num = num + 1;
	}
    }
    return num
}

/**
  Returns true if target is a subset of set, false otherwise

Ex: [1,3,2] is a subset of [1,2,3,4,5]
 **/


pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {

    let mut found = true;
    let mut count = 0;
    for element in target.iter() {
	/*println!("entered");*/

	for element2 in set.iter() {
	    if (*element == *element2) {
	    break;
	    }
	    if count == set.len()-1 {
		return false;
	    }
	    count = count + 1;

	    /*println!("end of loop");*/
	}
	count = 0;
	/*println!("found:{} ",found);*/
    }
    return true;
}

/**
  Returns the mean of elements in lst. If the list is empty, return None
  It might be helpful to use the fold method of the Iterator trait
 **/
pub fn mean(lst: &[f64]) -> Option<f64> {
    let total = 0;
    if lst.is_empty() {
	return None
    }
    let sum = lst.iter().fold(0.0, |acc, x| acc + x);
    let length = lst.len() as f64;
    return Some(sum / length)
}

/**
  Converts a binary number to decimal, where each bit is stored in order in the array

Ex: to_decimal of [1,0,1,0] returns 10
 **/

pub fn powered (num:i32, power:i32)-> i32 {
    if power == 0 {
	return 1;
    }
    let check =  num * powered(num, power-1);
    check
}

pub fn to_decimal(lst: &[i32]) -> i32 {
    let mut power :i32 = 0;
    let mut sum :i32 = 0;
    let mut count = 0;

    while count < lst.len(){
	let num =  lst[lst.len() - count - 1];
	if num == 1 {
	    sum = sum + powered (2, power);
	}
	power = power + 1;
	count= count + 1;
    }

    return sum;
}

/**
  Decomposes an integer into its prime factors and returns them in a vector
  You can assume factorize will never be passed anything less than 2

Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
 **/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut vec = Vec::new();
    let mut num = n;

    if n == 2 {
	vec.push(2);
	return vec;
    }

    for i in 2 .. n {
	while num % i == 0{
	    vec.push(i);
	    num = num / i;
	}

    }
    if num > 1 {
	vec.push(num);
    }
    return vec;
}

/**
  Takes all of the elements of the given slice and creates a new vector.
  The new vector takes all the elements of the original and rotates them,
  so the first becomes the last, the second becomes first, and so on.

EX: rotate [1,2,3,4] returns [2,3,4,1]
 **/

pub fn rotate(lst: &[i32]) -> Vec<i32> {
let mut vec = Vec :: new();

if lst.len() == 0{
return vec;
}

for i in 1 .. lst.len() {
vec.push(lst[i]);
}
vec.push(lst[0]);
return vec;
}

/**
  Returns true if target is a subtring of s, false otherwise
  You should not use the contains function of the string library in your implementation

Ex: "ace" is a substring of "rustacean"
 **/
pub fn substr(s: &String, target: &str) -> bool {
let mut count = 0;
if target.is_empty(){
return true;
}
if (target.len() > s.len()){
return false;
}
let bytes = s.as_bytes();
let bytes_target = target.as_bytes();

for (i, &item) in bytes.iter().enumerate(){
    if item == bytes_target[0] && target.len()== 1{
        return true;
        
    }
    if  item == bytes_target[0]{
    for j in 1 .. target.len(){
    if   bytes_target[j]  != bytes[i + j] {
    break;
    }   
    if j ==  target.len() -1{ 
    return true;
    }   


    }   
   
}
count = count + 1;

}
return false;
}









/**
  Takes a string and returns the first longest substring of consecutive equal characters

EX: longest_sequence of "ababbba" is Some("bbb")
EX: longest_sequence of "aaabbb" is Some("aaa")
EX: longest_sequence of "xyz" is Some("x")
EX: longest_sequence of "" is None
 **/
pub fn longest_sequence(s: &str) -> Option<&str> {
let string = s.to_string();
if s.is_empty(){
return None;
}
let bytes = s.as_bytes();
let mut last_character = bytes[0];
let mut count = 1;
let mut max = 0;
let mut position = 0;

for (i, &item) in
    bytes.iter().enumerate() {
        if item == last_character && i != 0{
            count = count + 1;
        }
        else{
        if count > max{
         max = count;
         position = i;
         println!("The count is {}", count);
         println!("the position is {}", position);
        }
        count = 1;
	/*why can I do this*/
        last_character = item; 
        }
            
        }
    
return Some(&s[position - max .. position]);
}
