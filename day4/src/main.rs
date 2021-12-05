use std::{
    collections::{HashMap, HashSet},
    num::ParseIntError,
    str::FromStr,
};

fn main() {
    let mut input = parse_input(input()).unwrap();
    match part1(&mut input) {
        Some(i) => println!("Part 1: {}", i),
        None => println!("No winner"),
    }

    match part2(&mut input) {
        Some(i) => println!("Part 2: {}", i),
        None => println!("No winner"),
    }
}

fn input() -> &'static str {
    include_str!("../../input/day4.txt")
}

fn part1(input: &mut Bingo) -> Option<usize> {
    for &number in input.numbers.iter() {
        for board in input.boards.iter_mut() {
            if board.mark_number(number) {
                return Some(board.sum_unmarked() * number);
            }
        }
    }
    return None;
}

fn part2(input: &mut Bingo) -> Option<usize> {
    let mut winners: HashSet<usize> = HashSet::new();
    let boards_count = input.boards.len();
    for &n in input.numbers.iter() {
        for (i, board) in input.boards.iter_mut().enumerate() {
            if winners.contains(&i) {
                continue;
            }
            if board.mark_number(n) {
                winners.insert(i);
                if winners.len() == boards_count {
                    return Some(board.sum_unmarked() * n);
                }
            }
        }
    }

    None
}

#[derive(Clone)]
struct Bingo {
    numbers: Vec<usize>,
    boards: Vec<Board>,
}

#[derive(Clone)]
struct Board {
    numbers: HashMap<usize, (usize, usize)>,
    locations: [[Square; 5]; 5],
}

impl Board {
    fn mark_number(&mut self, n: usize) -> bool {
        let &(row, col) = match self.numbers.get(&n) {
            Some(x) => x,
            None => return false,
        };

        self.locations[row][col].marked = true;

        self.row_complete(row) || self.col_complete(col)
    }

    fn row_complete(&self, row: usize) -> bool {
        self.locations[row].iter().all(|square| square.marked)
    }

    fn col_complete(&self, col: usize) -> bool {
        self.locations.iter().all(|row| row[col].marked)
    }

    fn sum_unmarked(&self) -> usize {
        self.locations.iter().fold(0, |tot, row| {
            tot + row.iter().fold(0, |row_tot, s| {
                if !s.marked {
                    row_tot + s.number
                } else {
                    row_tot
                }
            })
        })
    }
}

impl FromStr for Board {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines = s.lines().filter(|s| !s.is_empty());

        let mut locations = empty_locations();
        let mut numbers = HashMap::new();

        for (r, line) in lines.enumerate() {
            let nums = line.trim().split(' ').filter(|s| !s.is_empty());
            for (c, num) in nums.enumerate() {
                if r >= 5 || c >= 5 {
                    return Err(format!(
                        "index out of bounds: r={}, c={} in '{}'",
                        r, c, line
                    ));
                }

                let n: usize = num.parse().map_err(|e: ParseIntError| e.to_string())?;

                locations[r][c].number = n;
                numbers.insert(n, (r, c));
            }
        }

        Ok(Board { numbers, locations })
    }
}

fn empty_locations() -> [[Square; 5]; 5] {
    let mut locations = [[Square::default(); 5]; 5];
    for i in 0..5 {
        for j in 0..5 {
            locations[i][j].row = i;
            locations[i][j].col = j;
            locations[i][j].marked = false;
        }
    }
    locations
}

#[derive(Copy, Clone, Default)]
struct Square {
    number: usize,
    row: usize,
    col: usize,
    marked: bool,
}

fn parse_input(input: &str) -> Result<Bingo, String> {
    let mut iter = input.split("\n\n");
    let numbers_raw = iter.next().ok_or("empty input".to_string())?;

    let numbers = numbers_raw
        .split(',')
        .map(str::parse)
        .collect::<Result<_, _>>()
        .map_err(|e: ParseIntError| e.to_string())?;

    let boards = iter
        .filter(|s| !s.is_empty())
        .map(Board::from_str)
        .collect::<Result<_, _>>()?;

    Ok(Bingo { numbers, boards })
}
