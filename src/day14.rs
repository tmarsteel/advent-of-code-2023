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
    fn is_empty(&self) -> bool {
        match self {
            PlatformItem::Empty => true,
            _ => false,
        }
    }

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

#[derive(Debug, Clone, PartialEq)]
struct PlatformState {
    rows: Vec<Vec<PlatformItem>>,
}

impl PlatformState {
    fn tilt_north(&mut self) {
        let mut any_moved = true;
        while any_moved {
            // there is no do-while, so we have to do this uglyness
            any_moved = false;

            // loop rows windowed
            for row_index in 0..self.rows.len() - 1 {
                let row_window = self.rows.get_many_mut([row_index, row_index+1]).expect("Index math incorrect");
                for col_index in 0..row_window[0].len() {
                    if row_window[0][col_index].is_empty() && row_window[1][col_index].is_movable() {
                        row_window[0][col_index] = row_window[1][col_index].clone();
                        row_window[1][col_index] = PlatformItem::Empty;
                        any_moved = true;
                    }
                }
            }
        }
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

    fn n_columns(&self) -> usize {
        // just assume square, even though this is never checked anywhere :)
        self.rows.get(0).map(|row| row.len())
            .unwrap_or(0usize)
    }

    /**
     * Rotates the platform by 90 degrees clockwise
     */
    fn rotate_quarter_circle_clockwise(&self) -> PlatformState {
        let mut rows: Vec<Vec<PlatformItem>> = Vec::new();
        for column_index in 0..self.n_columns() {
            let mut new_row = self.rows.iter()
                .map(|row| row[column_index].clone())
                .collect::<Vec<_>>();
            new_row.reverse();
            rows.push(new_row);
        }

        PlatformState { rows }
    }

    fn cycle_times(&self, n_cycles: u32) -> PlatformState {
        let mut carry = self.clone();
        for n_cycle in 0..n_cycles {
            carry.tilt_north();
            carry = carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry = carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry = carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry = carry.rotate_quarter_circle_clockwise();

            if n_cycle % 1000 == 0 {
                println!("{} of {} cycles done", n_cycle, n_cycles)
            }
        }

        carry
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
    let (_, mut platform) = platform_state(&input_string).expect("Failed to parse input");
    println!("{}", platform.cycle_times(1000000000).north_beam_load());
}