use std::collections::{HashSet, VecDeque};
use std::iter::Extend;

fn main() {
    let mut input = parse_input(input());
    println!("Part 1: {}", part1(&mut input));
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: &mut Paper) -> usize {
    input.interpret_one();
    input.dots.len()
}

fn part2(input: &mut Paper) -> String {
    input.interpret_all();

    format!("{}", input)
}

type DotLocs = HashSet<(usize, usize)>;

#[derive(Clone, Copy)]
enum Instruction {
    FoldHorizontal(usize),
    FoldVertical(usize),
}

struct Paper {
    dots: DotLocs,
    instructions: VecDeque<Instruction>,
}

impl Paper {
    fn fold_horz(&mut self, x: usize) {
        let (below, mut above): (DotLocs, DotLocs) = self.dots.iter().partition(|&(dx, _)| dx > &x);

        above.extend(below.iter().map(|&(dx, dy)| (2 * x - dx, dy)));

        self.dots = above;
    }

    fn fold_vert(&mut self, y: usize) {
        let (below, mut above): (DotLocs, DotLocs) = self.dots.iter().partition(|&(_, dy)| dy > &y);

        above.extend(below.iter().map(|&(dx, dy)| (dx, 2 * y - dy)));

        self.dots = above;
    }

    fn interpret_one(&mut self) {
        let instruction = self.instructions.pop_front();
        match instruction {
            Some(Instruction::FoldHorizontal(x)) => self.fold_horz(x),
            Some(Instruction::FoldVertical(y)) => self.fold_vert(y),
            _ => {}
        }
    }

    fn interpret_all(&mut self) {
        while !self.instructions.is_empty() {
            self.interpret_one();
        }
    }
}

impl std::fmt::Display for Paper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = *self.dots.iter().map(|(x, _)| x).max().unwrap();
        let height = *self.dots.iter().map(|(_, y)| y).max().unwrap();
        let mut ch = vec![vec![' '; width + 1]; height + 1];

        for &(x, y) in self.dots.iter() {
            ch[y][x] = 'X';
        }

        let grid = ch.iter().fold(String::new(), |s, l| {
            format!("{}\n{}", s, l.iter().collect::<String>())
        });

        write!(f, "{}", grid)
    }
}

fn input() -> &'static str {
    include_str!("../../input/day13.txt")
}

fn parse_input(s: &str) -> Paper {
    let mut parts = s.split("\n\n");
    let dots_s = parts.next().unwrap();
    let instructions_s = parts.next().unwrap();

    let dots = dots_s
        .trim()
        .lines()
        .map(|l| {
            let mut nums = l.trim().split(',');
            (
                nums.next().unwrap().parse().unwrap(),
                nums.next().unwrap().parse().unwrap(),
            )
        })
        .collect();

    let instructions = instructions_s
        .trim()
        .lines()
        .map(|l| {
            let mut parts = l.split('=');
            let typ = parts.next().unwrap();
            let loc = parts.next().unwrap().parse().unwrap();
            if typ.ends_with('x') {
                Instruction::FoldHorizontal(loc)
            } else {
                Instruction::FoldVertical(loc)
            }
        })
        .collect();

    Paper { dots, instructions }
}
