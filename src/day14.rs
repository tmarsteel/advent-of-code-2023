use std::collections::{HashMap, HashSet};
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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

#[derive(Clone, Hash, Eq, PartialEq)]
enum MatrixRotation {
    ORIGINAL,
    ONCE,
    TWICE,
    THRICE,
}

impl MatrixRotation {
    fn swap_rows_cols<T>(matrix: &mut Matrix2<T>) {
        let tmp = matrix.n_rows;
        matrix.n_rows = matrix.n_cols;
        matrix.n_cols = tmp;
    }

    fn rotate_quarter_circle_clockwise<T>(matrix: &mut Matrix2<T>) {
        matrix.rotation = match matrix.rotation {
            MatrixRotation::ORIGINAL => MatrixRotation::ONCE,
            MatrixRotation::ONCE => MatrixRotation::TWICE,
            MatrixRotation::TWICE => MatrixRotation::THRICE,
            MatrixRotation::THRICE => MatrixRotation::ORIGINAL,
        };
        MatrixRotation::swap_rows_cols(matrix);
    }

    fn calculate_index<T>(&self, matrix: &Matrix2<T>, row: usize, col: usize) -> usize {
        match self {
            MatrixRotation::ORIGINAL => matrix.n_cols * row + col,
            MatrixRotation::ONCE => (matrix.n_cols - 1 - col) * matrix.n_rows + row,
            MatrixRotation::TWICE => (matrix.n_rows - 1 - row) * matrix.n_cols + col,
            MatrixRotation::THRICE => (matrix.n_rows * col) + (matrix.n_rows - 1 - row),
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct Matrix2<T> {
    elements: Vec<T>,
    n_rows: usize,
    n_cols: usize,
    rotation: MatrixRotation,
}

impl<T : Clone> Matrix2<T> {
    fn new(rows: Vec<Vec<T>>) -> Matrix2<T> {
        let n_rows = rows.len();
        let n_cols = rows.first().map(|r| r.len()).unwrap_or(0);
        for i in 0..n_rows {
            if rows[i].len() != n_cols {
                panic!("Given data is not a rectangle: row#{} has {} elements, different than the expected {}", i, rows[i].len(), n_cols);
            }
        }

        let data: Vec<T> = rows.iter().flatten().map(|e| e.clone()).collect();

        Matrix2 {
            elements: data,
            n_rows,
            n_cols,
            rotation: MatrixRotation::ORIGINAL,
        }
    }

    fn calculate_index(&self, row: usize, col: usize) -> usize {
        return self.rotation.calculate_index(self, row, col)
    }

    fn get(&self, row: usize, col: usize) -> &T {
        &self.elements[self.calculate_index(row, col)]
    }

    fn set(&mut self, row: usize, col: usize, item: T) {
        let index = self.calculate_index(row, col);
        self.elements[index] = item;
    }

    fn iter_row<'a>(&'a self, row: usize) -> impl Iterator<Item = &'a T> {
        self.elements.iter()
            .skip(self.calculate_index(row, 0))
            .take(self.n_cols)
    }

    fn rotate_quarter_circle_clockwise(&mut self) {
        MatrixRotation::rotate_quarter_circle_clockwise(self);
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct PlatformState {
    data: Matrix2<PlatformItem>,
}

impl PlatformState {
    fn new(rows: Vec<Vec<PlatformItem>>) -> PlatformState {
        PlatformState {
            data: Matrix2::new(rows),
        }
    }

    fn tilt_north(&mut self) {
        let mut any_moved = true;
        while any_moved {
            // there is no do-while, so we have to do this uglyness
            any_moved = false;

            // loop rows windowed
            for row_index in 0..self.data.n_rows - 1 {
                let row_index_next = row_index + 1;
                for col_index in 0..self.data.n_cols {
                    let row_item = self.data.get(row_index, col_index);
                    let next_item = self.data.get(row_index_next, col_index);
                    if row_item.is_empty() && next_item.is_movable() {
                        self.data.set(row_index, col_index, next_item.clone());
                        self.data.set(row_index_next, col_index, PlatformItem::Empty);
                        any_moved = true;
                    }
                }
            }
        }
    }

    fn north_beam_load(&self) -> u32 {
        let mut result: u32 = 0;
        for row_index in 0..self.data.n_rows {
            let row_load_factor = u32::try_from(self.data.n_rows - row_index).unwrap();
            result += self.data.iter_row(row_index)
                .map(|item| item.weight() * row_load_factor)
                .sum::<u32>()
        }

        result
    }

    /**
     * Rotates the platform by 90 degrees clockwise
     */
    fn rotate_quarter_circle_clockwise(&mut self) {
        self.data.rotate_quarter_circle_clockwise();
    }

    fn cycle_times(&self, n_cycles: u32) -> PlatformState {
        let mut states_seen: HashMap<PlatformState, u32> = HashMap::new();
        states_seen.insert(self.clone(), 0);

        let mut n_cycle: u32 = 0;
        let mut carry = self.clone();
        while n_cycle < n_cycles {
            carry.tilt_north();
            carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry.rotate_quarter_circle_clockwise();
            carry.tilt_north();
            carry.rotate_quarter_circle_clockwise();

            match states_seen.get(&carry) {
                Some(period_starts_at_index) => {
                    let period_length = n_cycle - period_starts_at_index;
                    println!("period detected at cycle {}: starts at {} of length {}", n_cycle, period_starts_at_index, period_length);
                    if period_length == 0 {
                        return carry
                    }

                    let cycles_remaining = n_cycles - n_cycle;
                    let cycles_into_period = cycles_remaining % period_length;
                    let index_to_return = period_starts_at_index + cycles_into_period - 1;
                    println!("cycles_remaining = {}, cycles_into_period = {}, index_to_return = {}", cycles_remaining, cycles_into_period, index_to_return);
                    return states_seen.iter()
                        .filter_map(|(state, index)| {
                            if *index == index_to_return {
                                Some(state)
                            } else {
                                None
                            }
                        })
                        .next()
                        .unwrap()
                        .clone();
                }
                None => {
                    states_seen.insert(carry.clone(), n_cycle);
                }
            }

            n_cycle = n_cycle + 1
        }

        panic!("Unreachable")
    }
}

impl Display for PlatformState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in 0..self.data.n_rows {
            for col in 0..self.data.n_cols {
                self.data.get(row, col).fmt(f)?
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
    Ok((input, PlatformState::new(rows)))
}

pub(crate) fn day14() {
    let input_string = fs::read_to_string("inputs/day14.txt")
        .expect("Couldnt read input");
    let (_, mut platform) = platform_state(&input_string).expect("Failed to parse input");

    let cycled = platform.cycle_times(1_000_000_000);
    println!("{}", cycled.north_beam_load());
}