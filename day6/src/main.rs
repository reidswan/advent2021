use std::{collections::HashMap, iter::repeat, num::ParseIntError};

fn main() {
    let mut input = parse_input(input()).unwrap();
    println!("Part 1: {}", part1(&mut input));
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: &mut LanternFishGroup) -> usize {
    for _ in 0..80 {
        input.tick()
    }

    input.count()
}

fn part2(input: &mut LanternFishGroup) -> usize {
    for _ in 80..256 {
        input.tick()
    }

    input.count()
}

fn input() -> &'static str {
    include_str!("../../input/day6.txt")
}

#[derive(Debug)]
struct LanternFishGroup {
    count_by_cycle: HashMap<u8, usize>,
}

impl LanternFishGroup {
    fn count(&self) -> usize {
        self.count_by_cycle.values().sum()
    }

    fn tick(&mut self) {
        let cycle_ends = self.count_by_cycle[&0];

        // move the counts at position i in their cycle to position i-1
        // effectively decreasing the cycle position for those fish by 1
        for i in 1..=8 {
            let count = self.count_by_cycle[&i];
            self.count_by_cycle.insert(i - 1, count);
        }

        // insert newly spawned lantern fish
        self.count_by_cycle.insert(8, cycle_ends);
        // reset cycle for those that hit 0
        *self.count_by_cycle.get_mut(&6).unwrap() += cycle_ends;
    }
}

fn parse_input(s: &str) -> Result<LanternFishGroup, String> {
    let mut count_by_cycle: HashMap<u8, usize> = (0..=8).zip(repeat(0)).collect();
    for c in parse_input_list(s).map_err(|e| e.to_string())? {
        *count_by_cycle.get_mut(&c).unwrap() += 1
    }

    Ok(LanternFishGroup { count_by_cycle })
}

fn parse_input_list(s: &str) -> Result<Vec<u8>, ParseIntError> {
    s.trim().split(',').map(str::parse).collect()
}
