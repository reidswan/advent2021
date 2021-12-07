use std::num::ParseIntError;

fn main() {
    let input = parse_input(input()).unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input))
}

fn part1(input: &Vec<usize>) -> usize {
    input
        .iter()
        .map(|&x| fuel_spend_p1(input, x))
        .min()
        .unwrap()
}

fn part2(input: &Vec<usize>) -> usize {
    let &min = input.iter().min().unwrap();
    let &max = input.iter().max().unwrap();

    (min..=max).map(|x| fuel_spend_p2(input, x)).min().unwrap()
}

fn parse_input(s: &str) -> Result<Vec<usize>, ParseIntError> {
    s.trim().split(',').map(str::parse).collect()
}

fn input() -> &'static str {
    include_str!("../../input/day7.txt")
}

fn fuel_spend_p1(source_positions: &Vec<usize>, dest: usize) -> usize {
    source_positions
        .iter()
        .map(|&x| (dest as isize - x as isize).abs() as usize)
        .sum()
}

fn fuel_spend_p2(source_positions: &Vec<usize>, dest: usize) -> usize {
    source_positions
        .iter()
        .map(|&x| fuel_required_p2(x, dest))
        .sum()
}

fn fuel_required_p2(from: usize, to: usize) -> usize {
    let distance = (from as isize - to as isize).abs() as usize;

    (distance * (distance + 1)) / 2
}
