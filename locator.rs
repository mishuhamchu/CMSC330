use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
  An optional definition of a Node struct you may find useful
 **/
struct Node<T> {
priority: i32,
	      data: T,
}

/**
  These traits are implemented for Nodes to make them comparable
 **/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
	self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
	self.priority == other.priority
    }
}

/**
  You must implement the above trait for the vector type
 **/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
      This functions pushes a given element onto the queue and
      reorders the queue such that the min heap property holds.
      See the project specifications for more details on how this
      works.
     **/


    /*remember; of we are borrowing a mutated value, we are not allowed to mutate the original*/
    fn enqueue(&mut self, ele: T) -> () {

	if self.contains(&ele) == false{
	    self.push(ele);      

	    if  self.len() != 1{
		let mut index = self.len() -1;  
		println!("the index is {}", index);
		println!("the parent index is {}", (index-1)/2);
		while (index != 0 && self[(index-1)/2] > self[index]){
		    println!("stuck in the loop");
		    self.swap((index-1)/2, index);
		    index = ((index-1)/2); 
		    println!("new index value is {}", index);
		}
		println!("exited");
	    }
	}
    }
    /**
      This function removes the root element from the queue and
      reorders the queue such that it maintains the min heap
      property.  See the project specifications for more details.
      You should return the deleted element in the form of an option.
      Return None if the queue was initially empty, Some(T) otherwise.
     **/
    fn dequeue(&mut self) -> Option<T> {
	println!("entered the dequeue method");
	if (self.len() == 1){ 
	    return self.pop();
	}   
	else if (self.len() == 0){ 
	    return None;
	}   
	else{
	    let mut last_index = self.len() -1; 
	    self.swap(0, last_index);
	    let mut curr_index = 0;
	    let mut left_child = 1;
	    let mut right_child = 2;
	    let mut small_child_index = 0;

	    if (right_child >= self.len() -1 ){

		if (left_child < self.len() - 1){
		    small_child_index = left_child;
		}
		else{
		    return self.pop();
		}

	    }
	    else{
		if self[left_child] > self[right_child]{
		    small_child_index = right_child;
		}
		else{
		    small_child_index = left_child;
		}
	    }


	    while (small_child_index < self.len() -1 && self[(curr_index)] > self[small_child_index]) {
		println!("the small child index is {}", small_child_index);  
		println!("the curr index(before swap) is {}", curr_index);
		self.swap(curr_index, small_child_index);
		curr_index = small_child_index;
		println!("the curr index is {}", curr_index);

		left_child = 2 * curr_index + 1;
		right_child = 2 * curr_index + 2;
		println!("the left child is {}", left_child);
		println!("the right child is {}", right_child);
		if (right_child >= self.len() -1 ){

		    if (left_child < self.len() - 1){
			small_child_index = left_child;
		    }
		    else{
			return self.pop();
		    }

		}
		else{
		    if self[left_child] > self[right_child]{
			small_child_index = right_child;
		    }
		    else{
			small_child_index = left_child;
		    }
		}

	    } 
	    return self.pop();
	}
    }   


    /**
      This function returns the element that would be removed
      if dequeue were called on the queue.  There should be no
      mutations to the queue.  Return the element in the form
      of an option.  Return None if the queue is empty, Some(T)
      otherwise.
     **/
    fn peek(&self) -> Option<&T> {
	if (self.is_empty()){
	    return None;
	}
	return Some(&self[0]);
    }
}


/**
  You must implement this function that computes the orthogonal
  distance between two coordinates.  Remember, orthogonal distance
  is not like Euclidean distance.  See the specifications for more
  details.
 **/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let mut x_distance = (p2.0 - p1.0);
    let mut y_distance = (p2.1 - p1.1);

    if (x_distance < 0){
	x_distance = x_distance * -1;
    }
    if (y_distance < 0){
	y_distance = y_distance * -1;
    }
    return y_distance + x_distance;
}

/**
  You must implement this function that determines which enemy Stark
  should battle and their coordinates.  You are given two hashmaps for
  allies and enemies.  Each maps a name to their current coordinates.
  You can assume that the allies hashmap will always have a name
  called "Stark" included.  Return the name and coordinates of the enemy
  Stark will battle in the form of a 3-tuple.  See the specifications
  for more details on how to choose which enemy.
 **/

pub fn target_checker<'a> (allies: &'a HashMap <&String, (i32,i32)>,
	enemies: &'a HashMap<&String, (i32,i32)>, villian_coordinates:(i32,i32), min_distance: &i32) -> bool{
    for (key, value) in allies.iter(){
	if distance(villian_coordinates, *value) < *min_distance{
	    return true;
	}
    }
    return false;
}


pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, 

	enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {	
    let mut distances = Vec :: new();
    let mut hash  = HashMap::new(); 
    let mut coordinates :(i32,i32) = (0,0); 
    let mut villian_coord :(i32, i32) = (0,0);
    for (key, value) in allies.iter(){
	if *(key.as_str()) == *"Stark"{
	    coordinates = *value;
	}
    }

    for (key, value) in enemies.iter(){ 
	hash.insert(distance(*value, coordinates), key);  
    }

    for (key, value) in hash.iter(){
	distances.enqueue(key);
    }

    let mut minimum = distances[0];
    /*hash.get_key_value returns an address*/
    let mut closest_villian = match hash.get_key_value(minimum){
	Some((value, name))=> name,
	    None => "",
    };

    for (key, value) in enemies.iter(){
	if *key == closest_villian{
	    villian_coord = *value;
	}
    }
    let check = target_checker (allies, enemies, villian_coord, minimum);		
  
  while(!distances.is_empty() && target_checker(allies,enemies, villian_coord, minimum)){
	minimum = match(distances.dequeue()){
	    Some((num))=>num,
		None => &0,
	};
	 closest_villian = match hash.get_key_value(minimum){
	    Some((value, name))=> name,
		None => "",
	};

	for (key, value) in enemies.iter(){
	    if *key == closest_villian{
		villian_coord = *value;
	    } 
	}

    }
    
    return (closest_villian, villian_coord.0, villian_coord.1);

}






