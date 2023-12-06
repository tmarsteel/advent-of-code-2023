use std::fs;
use std::str::FromStr;

pub fn day3() {
    let input_lines: Vec<InputLine> = fs::read_to_string("inputs/day3_part1_example.txt").expect("couldn't read input")
        .lines()
        .map(|line| line.parse::<InputLine>().expect(""))
        .collect();

    let sum = input_lines
        .iter()
        .map_windows(|[prev, focus, next]| {
            let symbols: Vec<Element> = [prev, next].iter()
                .flatten()
                .map(|l| l.elements.filter_map(|e| match e {
                    Element::Symbol(_) => Some(e),
                    _ => None,
                }))
                .collect();

            focus.elements
                .filter_map(|e| match e {
                    Element::Number(_) => if symbols.iter().any(|s| e.is_adjacent_to(s)) {
                        Some(e.value)
                    } else {
                        None
                    },
                    _ => None,
                })
                .sum();
        })
        .sum();
}

#[derive(Debug)]
struct Symbol {
    position_in_line: i32,
}

#[derive(Debug)]
struct Number {
    position_in_line: i32,
    text_length: i32,
    value: i32,
}

impl Number {
    fn is_adjacent_to(&self, symbol: &Symbol) -> bool {
        return symbol.position_in_line >= (self.position_in_line - 1)
            && symbol.position_in_line <= (self.position_in_line + self.text_length + 1)
        ;
    }
}

#[derive(Debug)]
#[derive(Eq, PartialEq)]
enum Element {
    Symbol(Symbol),
    Number(Number),
}

impl Element {
    fn is_adjacent_to(&self, other: Element) -> bool {
        return match self {
            Element::Symbol(self_symbol) => match other {
                Element::Symbol(_) => false,
                Element::Number(other_number) => other_number.is_adjacent_to(self_symbol),
            }
            Element::Number(self_number) => match other {
                Element::Symbol(other_symbol) => self_number.is_adjacent_to(&other_symbol),
                Element::Number(_) => false,
            }
        }
    }
}

#[derive(Debug)]
struct InputLine {
    elements: Vec<Element>,
}

impl FromStr for InputLine {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut elements: Vec<Element> = Vec::new();

        let mut current_index = 0;
        let mut current_number_starts_at: i32 = 0;
        let mut current_number: Vec<char> = Vec::new();
        for char in s.chars() {
            if char.is_digit(10) {
                if current_number.is_empty() {
                    current_number_starts_at = current_index;
                }
                current_number.push(char);
            } else {
                if !current_number.is_empty() {
                    let value = String::from_iter(current_number.iter()).parse::<i32>().expect("What number is this??");
                    elements.push(Element::Number(Number { value, position_in_line: current_number_starts_at, text_length: current_number.len() as i32 }));
                    current_number.clear();
                    current_number_starts_at = 0;
                }

                if char != '.' {
                    // assume symbol
                    elements.push(Element::Symbol(Symbol { position_in_line: current_index }));
                }
            }

            current_index = current_index + 1
        }

        if !current_number.is_empty() {
            let value = String::from_iter(current_number.iter()).parse::<i32>().expect("What number is this??");
            elements.push(Element::Number(Number { value, position_in_line: current_number_starts_at, text_length: current_number.len() as i32 }));
            current_number.clear();
            current_number_starts_at = 0;
        }

        Ok(InputLine { elements })
    }
}