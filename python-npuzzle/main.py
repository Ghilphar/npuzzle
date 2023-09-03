import os
import sys
import numpy as np
from src.solver import solve

# This function give for a given size of puzzle his solution in spiral.
def generate_final_state(size):
    final_state = np.zeros((size, size), dtype=int)
    current_number = 1
    top = 0
    bottom = size - 1
    left = 0
    right = size - 1

    while top <= bottom and left <= right:
        # Traverse from left to right
        for i in range(left, right+1):
            final_state[top][i] = current_number
            current_number += 1
        top += 1
        
        # Traverse from top to bottom
        for i in range(top, bottom+1):
            final_state[i][right] = current_number
            current_number += 1
        right -= 1
        
        # Traverse from right to left
        for i in range(right, left-1, -1):
            final_state[bottom][i] = current_number
            current_number += 1
        bottom -= 1
        
        # Traverse from bottom to top
        for i in range(bottom, top-1, -1):
            final_state[i][left] = current_number
            current_number += 1
        left += 1
    
    # Set the center to 0 for odd-sized puzzles
    if size % 2 == 1:
        final_state[size//2][size//2] = 0
    # Set the center for even-sized puzzles
    else:
        final_state[top][right] = 0
    
    return final_state
    

def print_usage():
    print("usage: python3 main.py <path_to_puzzle> <algorithm> <heuristic>")

def parse_puzzle(path_to_puzzle):
    # TODO: Check if there is no number twice in the puzzle or if there is a number missing
    with open(path_to_puzzle, "r") as f:
        lines = f.readlines()
    

    # Remove inlines comments and strip leading/trailing whitespaces
    lines = [line.split("#")[0].strip() for line in lines]

    # filter out empty lines
    lines = [line for line in lines if line]

    # Read size of the puzzle
    try:
        size = int(lines[0])
    except ValueError:
        print("error: invalid size")
        sys.exit(1)

    # Read the puzzle
    puzzle_elements = []
    for row in lines[1:]:
        new_puzzle_element = map(int, row.split())
        puzzle_elements.extend(new_puzzle_element)

    # Check if the number of elements is correct
    if len(puzzle_elements) != size * size:
        print("error: invalid number of elements")
        sys.exit(1)

    # Convert to a NumPy array
    puzzle_matrix = np.array(puzzle_elements).reshape(size, size)
    
    return size, puzzle_matrix

def parse_inputs():
    algorithms = ["astar"]
    heuristics = ["manhattan", "euclidean", "linear_conflict"]

    if len(sys.argv) != 4:
        print_usage()
        sys.exit(1)
    
    path_to_puzzle = sys.argv[1]
    algorithm = sys.argv[2]
    heuristic = sys.argv[3]

    if not os.path.isfile(path_to_puzzle):
        print("error: file does not exist")
        print_usage()
        sys.exit(1)
    if algorithm not in algorithms:
        print("error: invalid algorithm")
        print_usage()
        sys.exit(1)
    if heuristic not in heuristics:
        print("error: invalid heuristic")
        print_usage()
        sys.exit(1)
    
    # Parse the puzzle
    size, puzzle = parse_puzzle(path_to_puzzle)

    return size, puzzle, algorithm, heuristic

def count_inversions(puzzle_start, puzzle_solution):
    # Flatten the start grid to a 1D array and remove the zero
    puzzle_start_flat = puzzle_start.flatten()
    puzzle_start_flat = puzzle_start_flat[puzzle_start_flat != 0]
    # Flatten the solution grid to a 1D array and remove the zero
    puzzle_solution_flat = puzzle_solution.flatten()
    puzzle_solution_flat = puzzle_solution_flat[puzzle_solution_flat != 0]
    
    # Count inversions
    # Imagine we have a grid as :
    # [2,8,3],[1,6,4],[7,0,5]
    # And a solution grid as :
    # [1,2,3],[8,0,4],[7,6,5]
    # Flatten without 0 they give :
    # [2,8,3,1,6,4,7,5] and [1,2,3,8,4,7,6,5]
    # We count for 2: 1 inversion : tested with 8,3,1,6,4,7,5 : in the solution 2 is before 3,8,4,7,6,5 so there is 1 inversion who is 2,3
    # We count for 8: 2 inversion : tested with 3,1,6,4,7,5 : in the solution 8 is before 4,7,6,5 so there is 2 inversion who are 8,1 and 8,3
    # We count for 3: 1 inversion
    # We count for 1: 0 inversion
    # We count for 6: 2 inversion
    # We count for 4: 0 inverison
    # We count for 7: 0 inversion
    # We count for 5: 0 inversion 
    inversions = 0
    for i in range(len(puzzle_start_flat)):
        for j in range(i+1, len(puzzle_start_flat)):
            position_in_solution_i = np.where(puzzle_solution_flat == puzzle_start_flat[i])[0]
            position_in_solution_j = np.where(puzzle_solution_flat == puzzle_start_flat[j])[0]
            if position_in_solution_i > position_in_solution_j:
                inversions += 1

    return inversions

def puzzleSolvable(puzzle_start, puzzle_solution):
    # Find the position of the empty tile (0)
    empty_tile_row_puzzle_start = np.where(puzzle_start == 0)[0][0]
    empty_tile_row_puzzle_solution = np.where(puzzle_solution == 0)[0][0]
    
    # Count inversions
    inversions = count_inversions(puzzle_start, puzzle_solution)
    
    # Check solvability
    N = puzzle_start.shape[0]  # Size of the puzzle
    
    # If the puzzle is odd-sized, the number of inversions must be even
    if N % 2 == 1:
        return inversions % 2 == 0
    # If the puzzle is even-sized, the number of inversions must be even and the empty tile must be on an even row from the bottom
    else:
        return inversions % 2 == (empty_tile_row_puzzle_start - empty_tile_row_puzzle_solution) % 2

    

def main(puzzle_size, puzzle_start, algorithm, heuristic):
    puzzle_solution = generate_final_state(puzzle_size)

    # Check if the puzzle is solvable using the initial state and the solution
    isSolvable = puzzleSolvable(puzzle_start, puzzle_solution)
    if isSolvable:
        solve(puzzle_start, puzzle_solution, algorithm, heuristic)
    else:
        print("error: puzzle is not solvable")
        sys.exit(1)    

if __name__ == "__main__":
    puzzle_size, puzzle_start, algorithm, heuristic = parse_inputs()
    main(puzzle_size, puzzle_start, algorithm, heuristic)
    
    