use std::collections::HashSet;
use std::fmt::{Display, Formatter, Write};
use std::fs;
use nom::{
    IResult,
    branch::alt,
    character::complete::char,
};
use nom::character::complete::line_ending;
use nom::combinator::{all_consuming, eof};
use nom::multi::many1;

#[derive(Debug, Clone, PartialEq)]
enum PlatformItem {
    Empty,
    RoundRock,
    CubicRock,
}

impl PlatformItem {
    fn is_movable(&self) -> bool {
        match self {
            PlatformItem::RoundRock => true,
            _ => false,
        }
    }

    fn weight(&self) -> u32 {
        match self {
            PlatformItem::RoundRock => 1,
            _ => 0,
        }
    }
}

impl Display for PlatformItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            PlatformItem::Empty => f.write_char('.'),
            PlatformItem::RoundRock => f.write_char('O'),
            PlatformItem::CubicRock => f.write_char('#'),
        }
    }
}

#[derive(Debug, Clone)]
struct PlatformState {
    rows: Vec<Vec<PlatformItem>>,
}

impl PlatformState {
    fn tilted_north(&self) -> PlatformState {
        let mut copy = self.clone();
        let mut any_moved = true;
        while any_moved {
            // there is no do-while, so we have to do this uglyness
            any_moved = false;

            // loop rows windowed
            for i in 0..copy.rows.len() - 1 {
                let row_window = copy.rows.get_many_mut([i, i+1]).expect("Index math incorrect");
                let free_spots = indices_of(row_window[0], &PlatformItem::Empty);
                let movable_indices: Vec<_> = row_window[1]
                    .iter()
                    .enumerate()
                    .filter_map(|(index, below_item)| {
                        if below_item.is_movable() && free_spots.contains(&index) {
                            Some(index)
                        } else {
                            None
                        }
                    })
                    .collect();

                for movable_index in movable_indices {
                    row_window[0][movable_index] = row_window[1][movable_index].clone();
                    row_window[1][movable_index] = PlatformItem::Empty;
                    any_moved = true;
                }
            }
        }

        copy
    }

    fn north_beam_load(&self) -> u32 {
        self.rows.iter()
            .enumerate()
            .map(|(row_index, row)| {
                let row_load_factor = u32::try_from(self.rows.len() - row_index).unwrap();
                row.iter()
                    .map(|item| item.weight() * row_load_factor)
                    .sum::<u32>()
            })
            .sum::<u32>()
    }
}

impl Display for PlatformState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in self.rows.iter() {
            for item in row.iter() {
                item.fmt(f)?
            }
            f.write_char('\n')?
        }

        Ok(())
    }
}

fn platform_item_empty(input: &str) -> IResult<&str, PlatformItem> {
    let (input, _) = char('.')(input)?;
    Ok((input, PlatformItem::Empty))
}

fn platform_item_round_rock(input: &str) -> IResult<&str, PlatformItem> {
    let (input, _) = char('O')(input)?;
    Ok((input, PlatformItem::RoundRock))
}

fn platform_item_cubic_rock(input: &str) -> IResult<&str, PlatformItem> {
    let (input, _) = char('#')(input)?;
    Ok((input, PlatformItem::CubicRock))
}

fn platform_item(input: &str) -> IResult<&str, PlatformItem> {
    alt((platform_item_empty, platform_item_round_rock, platform_item_cubic_rock))(input)
}

fn input_row(input: &str) -> IResult<&str, Vec<PlatformItem>> {
    let (input, items) = many1(platform_item)(input)?;
    let (input, _) = alt((line_ending, eof))(input)?;
    Ok((input, items))
}

fn platform_state(input: &str) -> IResult<&str, PlatformState> {
    let (input, rows) = all_consuming(many1(input_row))(input)?;
    Ok((input, PlatformState { rows }))
}

pub(crate) fn day14() {
    let input_string = fs::read_to_string("inputs/day14.txt")
        .expect("Couldnt read input");
    let (_, platform) = platform_state(&input_string).expect("Failed to parse input");
    let platform = platform.tilted_north();
    println!("{}", platform);
    println!("{}", platform.north_beam_load());
}

fn indices_of<T : PartialEq>(items: &Vec<T>, needle: &T) -> HashSet<usize> {
    return items.iter()
        .enumerate()
        .filter_map(|(index, item)| {
            if item.eq(needle) {
                Some(index)
            } else {
                None
            }
        })
        .collect()
}