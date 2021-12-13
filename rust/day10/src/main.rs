fn main() {
    let input = parse_input(input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn part1(input: &Vec<String>) -> usize {
    let mut tot = 0;
    for line in input {
        let mut stack = Vec::new();

        for c in line.trim().chars() {
            if is_open(c) {
                stack.push(c)
            } else {
                match stack.pop() {
                    None => {
                        tot += score(c);
                        continue;
                    }
                    Some(o) => {
                        if !matches(o, c) {
                            tot += score(c);
                            continue;
                        }
                    }
                }
            }
        }
    }

    tot
}

fn part2(input: &Vec<String>) -> usize {
    let mut scores = vec![];

    'outer: for line in input {
        let mut stack = Vec::new();

        for c in line.trim().chars() {
            if is_open(c) {
                stack.push(c)
            } else {
                match stack.pop() {
                    None => {
                        continue 'outer;
                    }
                    Some(o) => {
                        if !matches(o, c) {
                            continue 'outer;
                        }
                    }
                }
            }
        }

        if !stack.is_empty() {
            scores.push(
                stack
                    .iter()
                    .rev()
                    .fold(0, |sum, &c| 5 * sum + score_p2(get_closer(c))),
            )
        }
    }

    scores.sort();

    scores[scores.len() / 2]
}

fn is_open(c: char) -> bool {
    "([{<".contains(c)
}

fn get_closer(open: char) -> char {
    match open {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => unreachable!(),
    }
}

fn matches(open: char, close: char) -> bool {
    get_closer(open) == close
}

fn score(c: char) -> usize {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn score_p2(c: char) -> usize {
    match c {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => 0,
    }
}

fn input() -> &'static str {
    include_str!("../../input/day10.txt")
}

fn parse_input(s: &str) -> Vec<String> {
    s.trim().lines().map(str::to_string).collect()
}
