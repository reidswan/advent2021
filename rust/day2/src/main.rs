use std::{num::ParseIntError, str::FromStr};

fn main() {
    let input = parse_input(input()).unwrap();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn input() -> &'static str {
    include_str!("../../input/day2.txt")
}

fn parse_input(input: &str) -> Result<Vec<Command>, String> {
    input.lines().map(str::parse).collect()
}

fn part1(input: &Vec<Command>) -> i64 {
    let pos = input.into_iter().fold(
        Position {
            distance: 0,
            depth: 0,
        },
        Position::act,
    );
    pos.depth * pos.distance
}

fn part2(input: &Vec<Command>) -> i64 {
    let pos = input.into_iter().fold(
        AimedPosition {
            distance: 0,
            depth: 0,
            aim: 0,
        },
        AimedPosition::act,
    );
    pos.depth * pos.distance
}

#[derive(Copy, Clone)]
struct Position {
    distance: i64,
    depth: i64,
}

impl Position {
    fn act(self, cmd: &Command) -> Self {
        match cmd.direction {
            Direction::Forward => Self {
                distance: self.distance + cmd.amount,
                ..self
            },
            Direction::Down => Self {
                depth: self.depth + cmd.amount,
                ..self
            },
            Direction::Up => Self {
                depth: self.depth - cmd.amount,
                ..self
            },
        }
    }
}

#[derive(Copy, Clone)]
struct AimedPosition {
    distance: i64,
    depth: i64,
    aim: i64,
}

impl AimedPosition {
    fn act(self, cmd: &Command) -> Self {
        match cmd.direction {
            Direction::Forward => Self {
                distance: self.distance + cmd.amount,
                depth: self.depth + cmd.amount * self.aim,
                ..self
            },
            Direction::Down => Self {
                aim: self.aim + cmd.amount,
                ..self
            },
            Direction::Up => Self {
                aim: self.aim - cmd.amount,
                ..self
            },
        }
    }
}

#[derive(Copy, Clone)]
enum Direction {
    Forward,
    Down,
    Up,
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "forward" => Ok(Direction::Forward),
            "up" => Ok(Direction::Up),
            "down" => Ok(Direction::Down),
            _ => Err(format!("not a direction: {}", s)),
        }
    }
}

#[derive(Copy, Clone)]
struct Command {
    direction: Direction,
    amount: i64,
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.trim().split_ascii_whitespace().collect::<Vec<_>>();
        if parts.len() != 2 {
            return Err(format!("malformed command: '{}'", s));
        }

        Ok(Self {
            direction: parts[0].parse()?,
            amount: parts[1].parse().map_err(|e: ParseIntError| e.to_string())?,
        })
    }
}
