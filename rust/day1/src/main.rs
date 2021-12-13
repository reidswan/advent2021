fn main() {
    let input = parse_input(input()).unwrap();

    match part1(&input) {
        Ok(i) => println!("Part 1: {}", i),
        Err(s) => println!("Something went wrong: {}", s),
    }

    match part2(&input) {
        Ok(i) => println!("Part 2: {}", i),
        Err(s) => println!("Something went wrong: {}", s),
    }
}

fn input() -> &'static str {
    include_str!("../../input/day1.txt")
}

fn parse_input(input: &str) -> Result<Vec<i64>, String> {
    input
        .lines()
        .filter(|s| !s.is_empty())
        .map(str::parse)
        .collect::<Result<Vec<i64>, _>>()
        .map_err(|e| format!("{:?}", e))
}

fn part1(input: &Vec<i64>) -> Result<i64, String> {
    let mut iter = input.iter();

    let first = iter.next().ok_or("Empty input".to_string())?;

    let (result, _) = iter.fold((0, first), |(count, prev), curr| {
        if curr > prev {
            (count + 1, curr)
        } else {
            (count, curr)
        }
    });

    Ok(result)
}

fn part2(input: &Vec<i64>) -> Result<i64, String> {
    if input.len() < 3 {
        return Err("input too short".to_string());
    }
    let mut iter = input.iter();
    let first = iter.next().unwrap();
    let second = iter.next().unwrap();
    let third = iter.next().unwrap();

    let (result, _) = iter.fold((0, (first, second, third)), |(count, (f, s, t)), curr| {
        if f < curr {
            (count + 1, (s, t, curr))
        } else {
            (count, (s, t, curr))
        }
    });

    Ok(result)
}
