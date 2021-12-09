use itertools::Itertools;
use std::collections::{HashMap, HashSet};

fn main() {
    let input = parse_input(input()).unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input).unwrap());
}

fn part1(input: &Vec<Vec<u32>>) -> u32 {
    input
        .iter()
        .enumerate()
        .map::<u32, _>(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|&(j, _)| is_low_point(input, i, j))
                .map(|(_, h)| h + 1)
                .sum()
        })
        .sum()
}

fn part2(input: &Vec<Vec<u32>>) -> Result<usize, String> {
    let mut basins: HashMap<(usize, usize), usize> = HashMap::new();

    for (i, row) in input.iter().enumerate() {
        for (j, &pt) in row.iter().enumerate() {
            if pt == 9 {
                continue;
            }

            let basin =
                determine_basin(input, i, j).ok_or(format!("No basin for point ({}, {})", i, j))?;

            *basins.entry(basin).or_insert(0) += 1;
        }
    }

    Ok(basins.values().sorted().rev().take(3).product())
}

fn determine_basin(matrix: &Vec<Vec<u32>>, i: usize, j: usize) -> Option<(usize, usize)> {
    if matrix[i][j] == 9 {
        return None;
    }

    let mut lowest_point_coords = (i, j);
    let mut lowest_point = matrix[i][j];
    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    visited.insert(lowest_point_coords);

    while !is_low_point(matrix, lowest_point_coords.0, lowest_point_coords.1) {
        let prev_pt = lowest_point_coords;
        for (i2, j2) in get_adjacent_points(matrix, lowest_point_coords.0, lowest_point_coords.1) {
            let pt = matrix[i2][j2];
            if pt == 9 {
                continue;
            } else if visited.contains(&(i2, j2)) {
                continue;
            } else if pt < lowest_point {
                lowest_point = pt;
                lowest_point_coords = (i2, j2);
                visited.insert(lowest_point_coords);
            }
        }

        if prev_pt == lowest_point_coords {
            return None;
        }
    }

    Some(lowest_point_coords)
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

fn is_low_point(matrix: &Vec<Vec<u32>>, i: usize, j: usize) -> bool {
    get_adjacent_points(matrix, i, j)
        .into_iter()
        .all(|(i2, j2)| matrix[i][j] < matrix[i2][j2])
}

fn parse_input(input: &str) -> Option<Vec<Vec<u32>>> {
    input
        .trim()
        .lines()
        .map(|l| l.trim().chars().map(|c| c.to_digit(10)).collect())
        .collect()
}

fn input() -> &'static str {
    include_str!("../../input/day9.txt")
}
