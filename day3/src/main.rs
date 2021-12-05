fn main() {
    let input = parse_input(input()).unwrap();

    println!("Part 1: {}", part1(&input));
    match part2(&input) {
        Ok(i) => println!("Part 2: {}", i),
        Err(s) => println!("Something went wrong: {}", s),
    }
}

#[derive(Copy, Clone)]
struct Count {
    his: usize,
    los: usize,
}

fn part1(input: &Vec<Bits>) -> usize {
    let mut counts = [Count { his: 0, los: 0 }; 12];
    for bits in input {
        for i in 0..12 {
            if bits[i] {
                counts[i].his += 1
            } else {
                counts[i].los += 1
            }
        }
    }

    let mut gamma = 0;
    let mut epsilon = 0;
    for count in counts {
        gamma *= 2;
        epsilon *= 2;
        if count.his > count.los {
            gamma += 1
        } else {
            epsilon += 1
        }
    }

    gamma * epsilon
}

fn bits_to_usize(b: &Bits) -> usize {
    b.iter().fold(0, |i, b| if *b { 2 * i + 1 } else { 2 * i })
}

fn last_bits_standing<'a>(
    input: &'a Vec<Bits>,
    bit_criteria_fn: fn(Count) -> bool,
) -> Option<&'a Bits> {
    let mut rem: Vec<&Bits> = input.iter().collect();
    for i in 0..12 {
        if rem.len() == 1 {
            return Some(rem[0]);
        }
        let counts = count_at_position(&rem, i);
        let bit_criteria = bit_criteria_fn(counts);
        rem = rem.into_iter().filter(|b| b[i] == bit_criteria).collect();
    }
    if rem.len() == 1 {
        return Some(rem[0]);
    }
    return None;
}

fn part2(input: &Vec<Bits>) -> Result<usize, String> {
    let oxygen = last_bits_standing(input, |count| count.his >= count.los)
        .ok_or("last_bits_standing for oxygen failed".to_string())?;
    let co2 = last_bits_standing(input, |count| count.his < count.los)
        .ok_or("last_bits_standing for CO2 failed")?;

    Ok(bits_to_usize(oxygen) * bits_to_usize(co2))
}

fn count_at_position(input: &Vec<&Bits>, position: usize) -> Count {
    let mut count = Count { his: 0, los: 0 };
    for bits in input {
        if bits[position] {
            count.his += 1
        } else {
            count.los += 1
        }
    }

    count
}

fn input() -> &'static str {
    include_str!("../../input/day3.txt")
}

type Bits = [bool; 12];

fn parse_bits(s: &str) -> Result<Bits, String> {
    let s = s.trim();
    if s.len() != 12 {
        return Err(format!("Expected 12 bits but got {} in '{}'", s.len(), s));
    }

    let mut result = [false; 12];
    for (i, c) in s.chars().enumerate() {
        match c {
            '1' => result[i] = true,
            '0' => result[i] = false,
            _ => return Err(format!("unexpected char '{}' in '{}'", c, s)),
        }
    }

    Ok(result)
}

fn parse_input(s: &str) -> Result<Vec<Bits>, String> {
    s.lines().map(parse_bits).collect()
}
