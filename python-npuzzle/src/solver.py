import numpy as np
import sys
import heapq # For priority queue
from src.config import algorithm_map, heuristic_map

def find_empty_tile(puzzle):
    position = np.where(puzzle == 0)
    row = position[0][0]
    column = position[1][0]
    return row, column  # Row, Column

def generate_neighbors(puzzle, empty_row, empty_col):
    neighbors = []
    size = puzzle.shape[0]
    up = (-1, 0)
    down = (1, 0)
    right = (0, 1)
    left = (0, -1)
    directions = [up, down, left, right]  # Up, Down, Left, Right

    for dx, dy in directions:
        # Add the direction vector to the empty tile's position to get the new position
        new_row, new_col = empty_row + dx, empty_col + dy

        # Check if the new position is valid
        if 0 <= new_row < size and 0 <= new_col < size:
            new_puzzle = puzzle.copy()
            # Swap the empty tile with the tile in the new position
            # Tuple unnpacking feature
            new_puzzle[empty_row][empty_col], new_puzzle[new_row][new_col] = \
                new_puzzle[new_row][new_col], new_puzzle[empty_row][empty_col]
            neighbors.append(new_puzzle)

    return neighbors

# Manhattan distance
# The Manhattan distance is the sum of the absolute differences between the current position and the goal position
def manhattan_distance(puzzle, goal):
    # Calculate the Manhattan distance
    distance = 0
    for i in range(puzzle.shape[0]):
        for j in range(puzzle.shape[1]):
            if puzzle[i][j] == 0:
                continue
            target_i, target_j = np.where(goal == puzzle[i, j])
            distance += abs(i - int(target_i)) + abs(j - int(target_j))
    return distance

# Linear Conflict
# Two tiles T1 and T2 are in linear conflict if they are in same row or column.
# And need to swap in order to go to the correct place.
def linear_conflict(puzzle, goal):
    distance = 0
    linear_conflict_count = 0

    for i in range(puzzle.shape[0]):
        for j in range(puzzle.shape[1]):
            if puzzle[i][j] == 0:
                continue
            target_i, target_j = np.where(goal == puzzle[i, j])
            distance += abs(i - int(target_i)) + abs(j - int(target_j))

            # Check for linear conflict in row
            # K index in the row
            for k in range(j + 1, puzzle.shape[1]):
                if puzzle[i][k] == 0:
                    continue
                target_k_i, target_k_j = np.where(goal == puzzle[i, k])
                if target_i == target_k_i and target_j > target_k_j:
                    linear_conflict_count += 1

            # Check for linear conflict in column
            # for l in range(i + 1, puzzle.shape[0]):
            #     if puzzle[l][j] == 0:
            #         continue
            #     target_l_i, target_l_j = np.where(goal == puzzle[l][j])
            #     if target_j == target_l_j and target_i > target_l_i:
            #         linear_conflict_count += 1

    return distance + 2 * linear_conflict_count


# Euclidean distance
# The Euclidean distance is the square root of the sum of the squared differences between the current position and the goal position
def euclidean_distance(puzzle, goal):
    # Calculate the Euclidean distance
    distance = 0
    for i in range(puzzle.shape[0]):
        for j in range(puzzle.shape[1]):
            if puzzle[i][j] == 0:
                continue
            target_i, target_j = np.where(goal == puzzle[i, j])
            distance += np.sqrt((i - int(target_i))**2 + (j - int(target_j))**2)
    return distance

# The python heapq module does not provide a direct way to update the priority of an item in the heap.
# So we will use a dictionary to keep track of the f(n) values of puzzles in the open set.
# 2 data structures: a priority queue and a dictionary

def a_star_algorithm(puzzle, goal, heuristic_func):
    total_states_analyzed = 0
    max_states_in_memory = 0

    # Convert the puzzle to a string
    start_puzzle_str = str(puzzle.tolist())
    # Convert the goal to a string
    goal_puzzle_str = str(goal.tolist())
    
    # Implement the A* algorithm using Manhattan distance as the heuristic
    start = (heuristic_func(puzzle, goal), start_puzzle_str, 0, None) # (f(n), puzzle, g(n), parent)
    # Initialize the open set with the start node, priority queue
    open_set = [start]
    # Heapify the open set
    heapq.heapify(open_set)

    # Initialize the closed set
    closed_set = set()

    # Create a dictionary to hold f(n) values for puzzles in the open set
    open_set_fn_values = {start_puzzle_str: start[0]} # {puzzle: f(n)}

    while open_set:
        # pop the puzzle with the lowest f(n) value
        current = heapq.heappop(open_set)
        # Increment the total_states_analyzed counter
        total_states_analyzed += 1
        # Update max_states_in_memory if necessary
        current_states_in_memory = len(open_set) + len(closed_set)
        if current_states_in_memory > max_states_in_memory:
            max_states_in_memory = current_states_in_memory

        #decompose the current node
        current_f, current_puzzle_str, current_g, current_parent = current

        # Add the current node to the closed set
        closed_set.add(current_puzzle_str)

        # Skip if current puzzle has been updated with a better f(n) in the open set
        if open_set_fn_values.get(current_puzzle_str) != current_f:
            continue

        # Check if current puzzle is the goal
        if current_puzzle_str == goal_puzzle_str:
            # Construct and return the solution path.
            solution_path = []
            while current is not None:
                current_f, current_puzzle_str, current_g, current_parent = current
                solution_path.append((current_puzzle_str, current_g, current_f))
                current = current_parent
            solution_path.reverse()
            print("Solution path :")
            num_steps_to_solution = 0
            for index, (puzzle_str, g_value, f_value) in enumerate(solution_path):
                num_steps_to_solution += 1
                print(f"g: {g_value}, f: {f_value}")
                print(np.array(eval(puzzle_str)))
                print()
            print(f"Numbers of steps to reach solution : {num_steps_to_solution}")
            print(f"Number of states represented in memory : {max_states_in_memory}")
            print(f"Numbers of states analyzed : {total_states_analyzed}")
            return solution_path

        # Convert string back to NumPy array
        current_puzzle = np.array(eval(current_puzzle_str))

        # Find the empty tile in the current puzzle state
        empty_row, empty_col = find_empty_tile(current_puzzle)
        # Generate the children of the current puzzle
        neighbors = generate_neighbors(current_puzzle, empty_row, empty_col)

        for neighbor in neighbors:
            # Convert the neighbor to a string
            neighbor_puzzle_str = str(neighbor.tolist())

            # Skip if the neighbor is in the closed set
            if neighbor_puzzle_str in closed_set:
                continue

            # Calculate f(n) for neighbor
            neighbor_g = current_g + 1
            neighbor_h = heuristic_func(neighbor, goal)
            neighbor_f = neighbor_g + neighbor_h

            ##neighbor_puzzle_str = np.array2string(neighbor)
            existing_fn = open_set_fn_values.get(neighbor_puzzle_str, float('inf'))
            if neighbor_f < existing_fn:
                #Update the f(n) value in the dictionary
                open_set_fn_values[neighbor_puzzle_str] = neighbor_f
                # Create the neighbor node
                neighbor_node = (neighbor_f, neighbor_puzzle_str, neighbor_g, current)
                # add or update neighbor in open_set
                heapq.heappush(open_set, neighbor_node)

    # Return None if no solution is found
    return None

algorithm_map['astar'] = a_star_algorithm
heuristic_map['manhattan'] = manhattan_distance
heuristic_map['euclidean'] = euclidean_distance
heuristic_map['linear_conflict'] = linear_conflict



def solve(puzzle, goal, algorithm, heuristic):
    # Solve the puzzle using the A* algorithm
    algo_func = algorithm_map.get(algorithm)
    heuristic_func = heuristic_map.get(heuristic)

    if algo_func is None:
        print("error: invalid algorithm")
        sys.exit(1)

    if heuristic_func is None:
        print("error: invalid heuristic")
        sys.exit(1)

    solution = algo_func(puzzle, goal, heuristic_func)
