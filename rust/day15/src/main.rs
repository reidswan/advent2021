use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;

fn main() {
    let input = parse_input(input());
    println!("Part 1: {}", part1(&input));
    println!("Part 1: {}", part2(&input));
}

fn part1(input: &HashMap<(isize, isize), isize>) -> isize {
    least_risky_path(input, (0, 0), (99, 99))
}

fn part2(input: &HashMap<(isize, isize), isize>) -> isize {
    let input = expand_input(input);
    let target = *input.keys().max().unwrap();
    least_risky_path(&input, (0, 0), target)
}

fn expand_input(input: &HashMap<(isize, isize), isize>) -> HashMap<(isize, isize), isize> {
    let (width, height) = (100, 100);
    input
        .iter()
        .map(|((x, y), v)| {
            (0..=4)
                .flat_map(|i| {
                    (0..=4).map(move |j| {
                        let new_val = (v - 1 + i + j) % 9 + 1;

                        ((x + height * i, y + width * j), new_val)
                    })
                })
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect()
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct PathPoint {
    position: (isize, isize),
    risk: isize,
}

impl Ord for PathPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.risk.cmp(&self.risk) {
            Ordering::Equal => self.position.cmp(&other.position),
            s => s,
        }
    }
}

impl PartialOrd for PathPoint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn least_risky_path(
    risks: &HashMap<(isize, isize), isize>,
    source: (isize, isize),
    target: (isize, isize),
) -> isize {
    let mut total_risk: HashMap<(isize, isize), isize> =
        risks.keys().map(|k| (*k, isize::MAX)).collect();
    total_risk.insert(source, 0);

    let mut q = BinaryHeap::new();
    q.push(PathPoint {
        position: source,
        risk: 0,
    });

    while let Some(PathPoint { position, risk }) = q.pop() {
        if position == target {
            return risk;
        }

        if risk > total_risk[&position] {
            continue;
        }
        let (i, j) = position;
        for neighbor in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)] {
            if !risks.contains_key(&neighbor) {
                continue;
            }

            let tot_risk = risk + risks[&neighbor];
            if tot_risk < total_risk[&neighbor] {
                total_risk.insert(neighbor, tot_risk);

                q.push(PathPoint {
                    risk: tot_risk,
                    position: neighbor,
                });
            }
        }
    }

    // not reachable :( *and* we're using in-band signalling! monstrous
    std::isize::MAX
}

fn parse_input(s: &str) -> HashMap<(isize, isize), isize> {
    s.trim()
        .lines()
        .enumerate()
        .map(|(i, l)| {
            l.trim()
                .chars()
                .enumerate()
                .map(|(j, c)| ((i as isize, j as isize), c.to_digit(10).unwrap() as isize))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect()
}

fn input() -> &'static str {
    include_str!("../../input/day15.txt")
}
