use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::HashMap, str::FromStr};

fn main() {
    let input = parse_input(input()).unwrap();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn input() -> &'static str {
    include_str!("../../input/day5.txt")
}

fn parse_input(s: &str) -> Result<Vec<Line>, String> {
    s.lines().map(str::parse).collect()
}

fn part1(input: &Vec<Line>) -> usize {
    let mut covered_counts: HashMap<(isize, isize), usize> = HashMap::new();

    for line in input {
        if !line.is_valid_part1() {
            continue;
        }

        for pt in line.covered_points() {
            *covered_counts.entry(pt).or_insert(0) += 1;
        }
    }

    covered_counts.values().filter(|&&c| c > 1).count()
}

fn part2(input: &Vec<Line>) -> usize {
    let mut covered_counts: HashMap<(isize, isize), usize> = HashMap::new();

    for line in input {
        for pt in line.covered_points() {
            *covered_counts.entry(pt).or_insert(0) += 1;
        }
    }

    covered_counts.values().filter(|&&c| c > 1).count()
}

struct Line {
    x_start: isize,
    y_start: isize,
    x_end: isize,
    y_end: isize,
}

impl Line {
    fn is_horizontal(&self) -> bool {
        self.y_start == self.y_end
    }

    fn is_vertical(&self) -> bool {
        self.x_start == self.x_end
    }

    fn is_valid_part1(&self) -> bool {
        self.is_horizontal() || self.is_vertical()
    }

    fn horz_direction(&self) -> isize {
        if self.x_start <= self.x_end {
            1
        } else {
            -1
        }
    }

    fn vert_direction(&self) -> isize {
        if self.y_start <= self.y_end {
            1
        } else {
            -1
        }
    }

    fn covered_points(&self) -> Vec<(isize, isize)> {
        if self.is_horizontal() {
            self.horz_covered_pts()
        } else if self.is_vertical() {
            self.vert_covered_pts()
        } else {
            self.diag_covered_pts()
        }
    }

    fn horz_covered_pts(&self) -> Vec<(isize, isize)> {
        let mut pts = Vec::new();
        let y = self.y_start;
        let dir = self.horz_direction();
        let mut x_curr = self.x_start - dir;

        while x_curr != self.x_end {
            x_curr += dir;
            pts.push((x_curr, y))
        }

        pts
    }

    fn vert_covered_pts(&self) -> Vec<(isize, isize)> {
        let mut pts = Vec::new();
        let x = self.x_start;
        let dir = self.vert_direction();
        let mut y_curr = self.y_start - dir;

        while y_curr != self.y_end {
            y_curr += dir;
            pts.push((x, y_curr))
        }

        pts
    }

    fn diag_covered_pts(&self) -> Vec<(isize, isize)> {
        let mut pts = Vec::new();

        let vert_dir = self.vert_direction();
        let mut y_curr = self.y_start - vert_dir;
        let horz_dir = self.horz_direction();
        let mut x_curr = self.x_start - horz_dir;

        while y_curr != self.y_end {
            y_curr += vert_dir;
            x_curr += horz_dir;
            pts.push((x_curr, y_curr))
        }

        pts
    }
}

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref LINE_RE: Regex = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
        }

        let caps = LINE_RE
            .captures(s.trim())
            .ok_or(format!("did not match regex: '{}'", s))?;

        Ok(Line {
            x_start: caps[1].parse().unwrap(),
            y_start: caps[2].parse().unwrap(),
            x_end: caps[3].parse().unwrap(),
            y_end: caps[4].parse().unwrap(),
        })
    }
}
