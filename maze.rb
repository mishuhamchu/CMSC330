
#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------

class Cell
attr_accessor :x, :y, :directions, :weights

	       def intitialize
	       end

def set (x, y, directions, weights)
    @x = x
    @y = y
    @directions = directions.clone()
    @weights = weights.clone()
    end

    end


def parse(file)
    puts "Not yet implemented"    

end

class Path
attr_accessor :x, :y, :directions, :name

def initialize(name,x,y,directions)
@name = name
@x = x
@y = y
@directions = directions
end
end

class Header
attr_accessor :sz, :sx, :sy, :ex, :ey
def initialize(sz, sx, sy, ex, ey)
@sz = sz
@sx = sx
@sy = sy
@ex = ex
@ey = ey
end

end

def num_openings(directions)
    return directions.length;
    end



def distance_cells(start_x, start_y, map, solve, end_x, end_y)
queue = Queue.new
distances = {}
seen = []
start_cell = map[start_x][start_y]
queue.push(start_cell)
seen[0] = (start_cell)
distances[map[start_x][start_y]] = 0
while (queue.empty? == false)
element = queue.pop
dirs = element.directions
if dirs != nil
if dirs.include? "u"
element_to_add = (map[element.x][element.y - 1])
if seen.include?(element_to_add) == false
seen.push(element_to_add)
distances[element_to_add] = distances[element] + 1
queue.push(element_to_add)
#if the element is in the seen set and it has a smaller distance at this 
#point, than the element will hash to this new distance
else
if distances[element_to_add] > distances[element] + 1
distances[element_to_add] = distances[element] + 1
end
end
end
if dirs.include? "d"
element_to_add = (map[element.x][element.y + 1])
if seen.include?(element_to_add) == false
seen.push(element_to_add)
distances[element_to_add] = distances[element] + 1
queue.push(element_to_add)
else
if distances[element_to_add] > distances[element] + 1
distances[element_to_add] = distances[element] + 1
end
end
end
if dirs.include? "l"
element_to_add = (map[element.x - 1][element.y])
if seen.include?(element_to_add) == false
seen.push(element_to_add)
distances[element_to_add] = distances[element] + 1
queue.push(element_to_add)
else
if distances[element_to_add] > distances[element] + 1
distances[element_to_add] = distances[element] + 1
end
end
end
if dirs.include? "r"
element_to_add = (map[element.x + 1][element.y])
if seen.include?(element_to_add) == false
seen.push(element_to_add)
distances[element_to_add] = distances[element] + 1
queue.push(element_to_add)
else
if distances[element_to_add] > distances[element] + 1
distances[element_to_add] = distances[element] + 1
end
end
end
end
end

if solve
end_cell = map[end_x][end_y]
if seen.include?(end_cell)
return true
else
return false
end
else
return distances
end
end

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

 def sort_cells(map)
 #p map
     
     sorted_cells = []
     sorted_cells_2 = []
     k = 0
      for i in 0 ... 5
     sorted_cells[i] = [i]
     end
 
     for i in 0 ... map.length     
     for j in 0 ... map.length
     ordered_pair = "(#{i},#{j})"
     #nil check
     if map[i][j].directions == nil
     sorted_cells[0].push(ordered_pair)
     else
     index = num_openings(map[i][j].directions) 
     sorted_cells[index].push(ordered_pair)
     end
     end
     end
     for i in 0 ... sorted_cells.length
        if sorted_cells[i].length != 1
     sorted_cells_2[k] = sorted_cells[i].clone()
     k += 1
     end
     end
     return sorted_cells_2
     end
 

def num_open(map)
num = 0
for i in 0 ... map.length
for j in 0 ... map.length
current_cell = map[i][j]
if current_cell.directions != nil && current_cell.directions.length == 4
num += 1
end
end
end
return num
end

def bridge_number(map)
down = 0
bridge = 0
#p map
right = 0
for i in 0...map.length()
right = 0
down = 0
for j in 0 ... map.length()
   
  ds = map[i][j].directions
   
   if ds == nil
   down = 0  
   end

   if ds != nil #start of nil case
   if ds.include? "d" 
    down+=1
    if down >= 2  
   
   bridge+=1
    end  #end of inner if   
    end #end of outer if

    if (ds.include? "d") == false 
    down = 0 
    end  
    end  #end of nil case

  ds2 = map[j][i].directions
   
  if ds2 == nil
  right = 0
  end

  if ds2 != nil #start of nil case
  if ds2.include? "r" 

    right += 1
    if right >= 2
 
  bridge +=1
  end #end of inner if
  end #end of outer if

  if (ds2.include? "r") == false
  right = 0
  end
  
  end #end of nil case
    end #end of inner loop
    end #end of outer loop

return bridge
    end 



def pretty_print(map,  header, path_enter, path_x, path_y, ds)
    pretty = []
# p map
#puts "start x :#{start_x}, start y :#{start_y}"
#puts "end x :#{end_x}, end y :#{end_y}"
#p pretty
   
#for each row
    start_x = header.sx.to_i
    start_y = header.sy.to_i
    end_x = header.ex.to_i
    end_y = header.ey.to_i
    arr = ["+"]
    arr2 = ["|"]
    size = header.sz.to_i
    
    for i in 0 .. size-1 
    for j in 0 .. size-1

    #we do j i because we want to go across the row
    dirs =  map[j][i].directions

    if (dirs == nil) || dirs.include?("u") == false
    arr.push("-")
    else
    arr.push(" ")
    end
    arr.push("+")
    if j == start_x && i == start_y
    arr2.push("s")
    elsif j == end_x && i == end_y
    arr2.push("e")
    else
    arr2.push(" ")
    end

    if dirs == nil || dirs.include?("r") == false
    arr2.push("|")
    else
    arr2.push(" ")
    end

    end#end of inner loop
    pretty.push(arr)
    pretty.push(arr2)
    arr = ["+"]
    arr2 = ["|"]

    end #end of outer loop
    arr = []
    arr.push("+")
    
    for j in 0 .. size - 1
    #dirs = map[size-1][j]
    arr.push("-")
    arr.push("+")
    end
    pretty.push(arr)
    if path_enter
    x = path_x.to_i * 2 + 1
    y = path_y.to_i * 2 + 1
    if (x == end_x*2 + 1) && (y == end_y*2 + 1)
    pretty[y][x] = "E"
    elsif (x == start_x*2 + 1) && (y == start_y*2 + 1)
    pretty[y][x] = "S"
    else pretty[y][x] = "*"
    end


    ds_characters = ds.chars
    ds_characters.each {|d|
	if d == "u"
	    y -= 2
		pretty[y][x] = "*"
		elsif d == "d"
		y += 2 
		pretty[y][x] = "*" 
		elsif d == "l"
		x -= 2
		pretty[y][x] = "*"
		elsif d == "r"
		x += 2
		pretty[y][x] = "*"
		end
		if x == end_x*2 + 1 && y == end_y*2 + 1
		    pretty[y][x] = "E"
			elsif x == start_x*2 + 1 && y == start_y*2 + 1
			pretty[y][x] = "S"
		else pretty[y][x] = "*"
		    end

    }
end
return pretty
    end

def path_weights(map, paths, print, header)
hash_path = {}
hash_path_names = {}
arr = []
coordinates = []
#p paths
paths.each{|path|

total_weight = 0
x_num = path.x.to_i
y_num = path.y.to_i
arr = []
if path.directions != nil
#loops through each of the characters in the path directions
path.directions.chars.each{|dir|
current_cell = map[x_num][y_num]
if current_cell.directions != nil && (current_cell.directions.include?(dir) == true)
total_weight += findWeightinCell(x_num, y_num, dir, map)
if dir == "d"
y_num += 1
elsif dir == "u"
y_num -= 1
elsif dir == "l"
x_num -= 1
elsif dir == "r"
x_num += 1
end
else
total_weight = nil
break 
end
}
else
total_weight = nil
end
if total_weight != nil
formatted = "%10.4f" % total_weight
hash_path[formatted] = path.name
hash_path_names[path.name] = []
hash_path_names[path.name][0] = path.x
hash_path_names[path.name][1] = path.y
hash_path_names[path.name][2] = path.directions
end
}
#p hash_path

sorted = hash_path.keys.sort
sorted.each { |key|
 arr.push( "#{key} #{hash_path[key]}")
}

if print
key = hash_path[sorted[0]]
coordinates = hash_path_names[key]

if coordinates != nil
return pretty_print(map,  header, true, coordinates[0], coordinates[1], coordinates[2])
else
return pretty_print(map,  header, false, nil, nil, nil)

end
else
#p sorted.length
if sorted.length != 0
return arr
else
return "none"
end
end
end

def findWeightinCell(x, y, direction, map)
    dirs = map[x][y].directions
    i = 0 
    while (dirs[i].eql?direction) == false 
    i+=1
    end
    weight = map[x][y].weights[i]

    return weight.to_f
    end

def read_and_print_simple_file(file, command)
    line = file.gets
    paths = [] 
    entered = nil   
    if line == nil then return end

# read 1st line, must be maze header
sz, sx, sy, ex, ey = line.split(/\s/)
header = Header.new(sz,sx,sy,ex,ey)

#puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"


#create a 2d array of cells
map = Array.new(sz.to_i) {  Array.new(sz.to_i, nil)}
for i in 0 ... map.length
for j in 0 ... map.length
map[i][j] = Cell.new
end
end

# read additional lines
    while line = file.gets do #start of while loop
   
# begins with "path", must be path specification
    if line[0...4] == "path"
      entered = 0
    p, name, x, y, ds = line.split(/\s/)
  
 #adds to the paths array
   new_path = Path.new(name,x,y,ds)
    paths.push(new_path)
      
    else

  x, y, ds, w = line.split(/\s/,4)
    
    if ds != nil && ds != ""
      ws = w.split(/\s/)
  end
     
    #creating the cell
    cell = Cell.new
    cell.set(x.to_i, y.to_i, ds, ws)
    map[x.to_i] [y.to_i] = cell 

    end  
    end

# puts bridge
    if command == "bridge" 
    return bridge_number(map)
    elsif command == "open" 
    return num_open(map) 

#puts open
    elsif command == "sortcells"
    sort_arr = []
    ordered_pairs = sort_cells(map)   
    ordered_pairs.each{ |ordered_pair| 
	ordered_pair = ordered_pair.join(",")    
	    sort_arr.push("#{ordered_pair}")
    } 
return  sort_arr
elsif command == "print"
if entered
arr =  path_weights(map, paths, true, header)
else
arr =  pretty_print(map,header,false, nil, nil, nil ) 
end
#p arr
    arr2 = []
    arr.each{ |e|
	e = e.join()
	    arr2.push(e)

    }
    returned_str = ""
    i = 0 
    arr2.each{ |element|
    returned_str.concat(element) 
    if i != arr2.length-1
    returned_str.concat("\n")
    end
    i+=1
    }
return returned_str

elsif command == "paths" 
if entered
return path_weights(map, paths, false, header)
else
return "none" 
end
#beginning of distance 
elsif command == "distance"
distances = distance_cells(sx.to_i, sy.to_i, map, nil, nil, nil)

distance_arr = []

max = 0

for i in 0 ... distances.keys.length()
if distances[distances.keys[i]] > max
max = distances[distances.keys[i]]  
end
end

for i in 0 ... max + 1
distance_arr[i] = ["#{i}"]
end


#p map
for i in 0 ... map.length
for j in 0 ... map.length
index = distances[map[i][j]]
if index != nil
distance_arr[index].push("(#{i},#{j})")
end
end
end
str = ""
i = 0
distance_arr.each{ |pair|
pair = pair.join(",")
str.concat("#{pair}")
if i != distance_arr.length-1
str.concat("\n")
end
i+=1
}
return str
#end of distance
elsif command == "solve"
solve = distance_cells(sx.to_i, sy.to_i, map, true, ex.to_i, ey.to_i)
return solve
end
end

#----------------------------------

    def main(command_name, file_name)
maze_file = open(file_name)

# perform command
    case command_name
    when "parse"
parse(maze_file)
    when "print"
read_and_print_simple_file(maze_file, command_name)
    when "open"
return read_and_print_simple_file(maze_file, command_name)
    when "bridge"
num = read_and_print_simple_file(maze_file, command_name)
    return num
    when "sortcells"
read_and_print_simple_file(maze_file, command_name)
    when "paths"
read_and_print_simple_file(maze_file, command_name)
    when "distance"
read_and_print_simple_file(maze_file, command_name)
   when "solve"
  read_and_print_simple_file(maze_file, command_name) 
  else
    fail "Invalid command"
    end

    end

