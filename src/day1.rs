use std::fs;

pub(crate) fn day1() {
    let sum: u32 = fs::read_to_string("inputs/day1.txt")
        .expect("Couldn't read day 1 inputs")
        //"six1mpffbnbnnlxthree"
        .lines()
        .map(|line| {
            let option_a = get_calibration_number(&mut DigitIterator::from_str(line, false));
            let option_b = get_calibration_number(&mut DigitIterator::from_str(line, true));
            if option_a != option_b {
                println!("difference in \"{}\": {} old, {} new", line, option_a, option_b)
            }
            option_b
        })
        .sum();

    println!("{}", sum)
}

fn get_calibration_number(digits: &mut dyn Iterator<Item = u32>) -> u32 {
    let digit1 = digits.next().expect("Did not find a digit on this line");
    let digit2 = digits.last().unwrap_or(digit1);
    return digit1 * 10 + digit2;
}

static WRITTEN_DIGITS: &'static [&str] = &[
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
];

struct DigitIterator {
    chars: Box<[char]>,
    next_index: usize,
    support_written: bool
}

impl DigitIterator {
    fn from_str(str: &str, support_written: bool) -> DigitIterator {
        let chars = str.chars().collect::<Box<[char]>>();
        return DigitIterator { chars, next_index: 0, support_written }
    }
}

impl Iterator for DigitIterator {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        while self.next_index < self.chars.len() {
            let next_char: char = self.chars[self.next_index];
            if next_char.is_digit(10) {
                self.next_index = self.next_index + 1;
                return Some(String::from_iter([next_char].iter()).parse::<u32>().unwrap())
            }

            if self.support_written {
                for written_digit_index in 0..WRITTEN_DIGITS.len() {
                    let written_digit = WRITTEN_DIGITS[written_digit_index];

                    if self.next_index + written_digit.len() > self.chars.len() {
                        continue
                    }

                    let next_chars = String::from_iter(self.chars[self.next_index..self.next_index + written_digit.len()].iter());
                    if next_chars == written_digit {
                        self.next_index += written_digit.len();
                        return Some((written_digit_index + 1) as u32);
                    }
                }
            }

            self.next_index = self.next_index + 1
        }

        return None
    }
}
