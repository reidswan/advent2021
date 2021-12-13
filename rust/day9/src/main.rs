use itertools::Itertools;
use std::collections::{HashMap, HashSet};

fn main() {
    let mut input = parse_input(input()).unwrap();
    println!("Part 1: {}", part1(&mut input));
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: &mut BasinMap) -> u32 {
    input
        .compute_low_points()
        .iter()
        .map(|&loc| input.get_val(loc) + 1)
        .sum()
}

fn part2(input: &mut BasinMap) -> usize {
    // let mut basins = HashMap::new();
    let mut basin_counts = HashMap::new();

    let rows = input.points.len();
    let cols = input.points[0].len();

    for i in 0..rows {
        for j in 0..cols {
            if input.get_val((i, j)) == 9 {
                continue;
            }
            let basin = input.get_basin((i, j));
            *basin_counts.entry(basin).or_insert(0) += 1;
        }
    }

    basin_counts.values().sorted().rev().take(3).product()
}

type Coord = (usize, usize);

struct BasinMap {
    points: Vec<Vec<u32>>,
    low_points: HashMap<Coord, bool>,
    basins: HashMap<Coord, Coord>,
}

impl BasinMap {
    fn new(points: Vec<Vec<u32>>) -> Self {
        Self {
            points,
            low_points: HashMap::new(),
            basins: HashMap::new(),
        }
    }

    fn compute_low_points(&mut self) -> HashSet<(usize, usize)> {
        let rows = self.points.len();
        let cols = self.points[0].len();
        for i in 0..rows {
            for j in 0..cols {
                self.is_low_point((i, j));
            }
        }

        self.low_points
            .iter()
            .filter(|(_, is_low)| **is_low)
            .map(|c| c.0.clone())
            .collect()
    }

    fn is_low_point(&mut self, location: Coord) -> bool {
        if self.low_points.contains_key(&location) {
            return self.low_points[&location];
        }

        let (x, y) = location;
        let is_low_point = get_adjacent_points(&self.points, x, y)
            .into_iter()
            .all(|(x2, y2)| self.points[x][y] < self.points[x2][y2]);

        self.low_points.insert(location, is_low_point);

        is_low_point
    }

    fn get_val(&self, (x, y): Coord) -> u32 {
        self.points[x][y]
    }

    fn get_basin(&mut self, location: Coord) -> Option<Coord> {
        if self.basins.contains_key(&location) {
            return Some(self.basins[&location]);
        }

        let mut visited = HashSet::new();
        self.compute_basin_rec(location, &mut visited)
    }

    fn compute_basin_rec(
        &mut self,
        location: Coord,
        visited: &mut HashSet<Coord>,
    ) -> Option<Coord> {
        if self.basins.contains_key(&location) {
            return Some(self.basins[&location]);
        }

        if self.get_val(location) == 9 {
            return None;
        }

        if self.is_low_point(location) {
            self.basins.insert(location, location);
            return Some(location);
        }

        visited.insert(location);

        let (x, y) = location;
        for adj_loc in get_adjacent_points(&self.points, x, y) {
            if self.get_val(adj_loc) == 9 {
                continue;
            } else if visited.contains(&adj_loc) {
                continue;
            } else if let Some(basin) = self.compute_basin_rec(adj_loc, visited) {
                self.basins.insert(location, basin);
                return Some(basin);
            }
        }

        return None;
    }
}

fn get_adjacent_points(matrix: &Vec<Vec<u32>>, i: usize, j: usize) -> Vec<(usize, usize)> {
    let mut adjacent_pts = vec![];
    if i > 0 {
        adjacent_pts.push((i - 1, j));
    }
    if j > 0 {
        adjacent_pts.push((i, j - 1))
    }
    if i < matrix.len() - 1 {
        adjacent_pts.push((i + 1, j));
    }
    if j < matrix[i].len() - 1 {
        adjacent_pts.push((i, j + 1));
    }
    adjacent_pts
}

fn parse_input(input: &str) -> Option<BasinMap> {
    let pts = input
        .trim()
        .lines()
        .map(|l| l.trim().chars().map(|c| c.to_digit(10)).collect())
        .collect::<Option<_>>()?;

    Some(BasinMap::new(pts))
}

fn input() -> &'static str {
    include_str!("../../input/day9.txt")
}
