use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

fn main() {
    let input = parse_input(input()).unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input))
}

fn part1(input: &Vec<InputLine>) -> usize {
    // 1: 2 segments; 4: 4 segments; 7: 3 segments; 8: 7 segments
    let segments = vec![2, 4, 3, 7];
    input
        .iter()
        .map(|i| {
            i.targets
                .iter()
                .filter(|s| segments.contains(&s.len()))
                .count()
        })
        .sum()
}

fn part2(input: &Vec<InputLine>) -> usize {
    input.iter().map(InputLine::compute_target).sum()
}

fn input() -> &'static str {
    include_str!("../../input/day8.txt")
}

fn parse_input(s: &str) -> Result<Vec<InputLine>, String> {
    s.lines().map(str::parse).collect()
}

struct InputLine {
    patterns: Vec<HashSet<char>>,
    targets: Vec<HashSet<char>>,
}

fn hashset_as_string(input: &HashSet<char>) -> String {
    String::from_iter(input.iter().sorted())
}

impl InputLine {
    fn compute_target(&self) -> usize {
        let digits = self.determine_digits();
        self.targets
            .iter()
            .fold(0, |tot, t| 10 * tot + digits[&hashset_as_string(t)])
    }

    fn determine_digits<'a>(&'a self) -> HashMap<String, usize> {
        let mut digit_patterns = HashMap::new();

        let pats_by_len = self.patterns.iter().into_group_map_by(|s| s.len());

        let one = pats_by_len[&2][0];
        let four = pats_by_len[&4][0];
        let seven = pats_by_len[&3][0];
        let eight = pats_by_len[&7][0];

        digit_patterns.insert(hashset_as_string(one), 1);
        digit_patterns.insert(hashset_as_string(four), 4);
        digit_patterns.insert(hashset_as_string(seven), 7);
        digit_patterns.insert(hashset_as_string(eight), 8);

        let mut six = &HashSet::new();
        // determine '0', '6' and '9' - '9' is a super set of '4', '0' is a superset of '7', '6' is neither
        for s in pats_by_len[&6].iter() {
            if s.is_superset(four) {
                digit_patterns.insert(hashset_as_string(s), 9);
            } else if s.is_superset(seven) {
                digit_patterns.insert(hashset_as_string(s), 0);
            } else {
                six = s;
                digit_patterns.insert(hashset_as_string(s), 6);
            }
        }

        // '3' is a superset of '7' and '5' is a subset of '6'; '2' is neither
        for s in pats_by_len[&5].iter() {
            if s.is_superset(seven) {
                digit_patterns.insert(hashset_as_string(s), 3);
            } else if s.is_subset(six) {
                digit_patterns.insert(hashset_as_string(s), 5);
            } else {
                digit_patterns.insert(hashset_as_string(s), 2);
            }
        }

        digit_patterns
    }
}

impl FromStr for InputLine {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split('|');
        let patterns = split
            .next()
            .unwrap()
            .trim()
            .split(' ')
            .map(|s| HashSet::from_iter(s.chars()))
            .collect();
        let targets = split
            .next()
            .ok_or(format!("No '|' in input"))?
            .trim()
            .split(' ')
            .map(|s| HashSet::from_iter(s.chars()))
            .collect();

        Ok(InputLine { patterns, targets })
    }
}
