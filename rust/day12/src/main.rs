use std::collections::{HashMap, HashSet};

fn main() {
    let input = parse_input(input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input))
}

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, Hash)]
struct Cave(String);

impl Cave {
    fn is_big(&self) -> bool {
        self.0.to_uppercase() == self.0
    }

    fn is_small(&self) -> bool {
        self.0.to_ascii_lowercase() == self.0
    }

    fn is_start(&self) -> bool {
        &self.0 == "start"
    }

    fn is_end(&self) -> bool {
        &self.0 == "end"
    }
}

fn part1(g: &Graph) -> usize {
    count_paths(g, &Cave("start".into()), &HashSet::new())
}

fn part2(g: &Graph) -> usize {
    count_paths_p2(g, &Cave("start".into()), &HashSet::new(), false)
}

type Graph = HashMap<Cave, HashSet<Cave>>;

fn count_paths<'a>(graph: &'a Graph, current: &'a Cave, visited_small: &HashSet<Cave>) -> usize {
    graph
        .get(current)
        .unwrap()
        .iter()
        .filter(|i| i.is_big() || !visited_small.contains(i))
        .map(|c| {
            if c.is_end() {
                1
            } else {
                let mut vs = visited_small.clone();
                vs.insert(current.clone());
                count_paths(graph, c, &vs)
            }
        })
        .sum()
}

fn count_paths_p2<'a>(
    graph: &'a Graph,
    current: &'a Cave,
    visited_small: &HashSet<Cave>,
    has_double_visited: bool,
) -> usize {
    graph
        .get(current)
        .unwrap()
        .iter()
        .filter(|c| {
            if c.is_start() {
                false
            } else if !has_double_visited {
                true
            } else {
                c.is_big() || !visited_small.contains(c)
            }
        })
        .map(|c| {
            if c.is_end() {
                1
            } else {
                if c.is_small() {
                    let has_double_visited = has_double_visited || visited_small.contains(c);
                    let mut vs = visited_small.clone();
                    vs.insert(c.clone());
                    count_paths_p2(graph, c, &vs, has_double_visited)
                } else {
                    count_paths_p2(graph, c, visited_small, has_double_visited)
                }
            }
        })
        .sum()
}

fn input() -> &'static str {
    include_str!("../../input/day12.txt")
}

fn parse_input(s: &str) -> Graph {
    let edges = s.lines().map(|l| {
        let mut parts = l.trim().split('-');
        (
            Cave(parts.next().unwrap().to_string()),
            Cave(parts.next().unwrap().to_string()),
        )
    });

    let mut graph = HashMap::new();

    for (v1, v2) in edges {
        graph
            .entry(v1.clone())
            .or_insert(HashSet::new())
            .insert(v2.clone());
        graph.entry(v2).or_insert(HashSet::new()).insert(v1);
    }

    graph
}
