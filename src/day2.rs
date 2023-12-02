use std::cmp::max;
use std::fmt::Display;
use std::fs;
use std::str::FromStr;

pub fn day2() {
    let games = read_input("inputs/day2.txt").expect("Invalid input format");

    // part 1
    let bag = CubeObservation {
        n_red: 12,
        n_green: 13,
        n_blue: 14,
    };
    let sum: i32 = games.iter()
        .filter(|g| g.is_possible_given_bag(&bag))
        .map(|g| g.id)
        .sum();

    // part2
    let sum: i32 = games.iter()
        .map(|g| g.minimal_bag().power())
        .sum();

    println!("{}", sum);
}

fn read_input(path: &str) -> Result<Vec<Game>, String> {
    let mut games: Vec<Game> = Vec::new();
    for line in fs::read_to_string(path).expect("Couldn't read day2 inputs").lines() {
        games.push(line.parse::<Game>()?);
    }

    return Ok(games)
}

#[derive(Debug)]
struct Game {
    id: i32,
    observations: Vec<CubeObservation>,
}

impl Game {
    fn is_possible_given_bag(&self, bag: &CubeObservation) -> bool {
        for observation in self.observations.iter() {
            if observation.n_red > bag.n_red || observation.n_blue > bag.n_blue || observation.n_green > bag.n_green {
                return false
            }
        }

        return true
    }

    fn minimal_bag(&self) -> CubeObservation {
        let mut bag = CubeObservation { n_red: 0, n_green: 0, n_blue: 0 };
        for observation in self.observations.iter() {
            bag.n_red = max(bag.n_red, observation.n_red);
            bag.n_green = max(bag.n_green, observation.n_green);
            bag.n_blue = max(bag.n_blue, observation.n_blue);
        }

        return bag
    }
}

impl FromStr for Game {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut colon_split = s.split(':');
        let game_id_part = colon_split.next().unwrap();
        let observations_part = colon_split.next().unwrap();

        let game_id_str = game_id_part.split_at(5).1;
        let game_id = match game_id_str.parse::<i32>() {
            Ok(id) => id,
            Err(_) => {
                return Err(format!("This game id is invalid: {}", game_id_part.split_at(4).1));
            },
        };

        let mut observations: Vec<CubeObservation> = Vec::new();
        for observation_str in observations_part.split(';') {
            let observation = observation_str.trim().parse::<CubeObservation>()?;
            observations.push(observation)
        }

        Ok(Game { id: game_id, observations })
    }
}

#[derive(Debug)]
struct CubeObservation {
    n_red: i32,
    n_blue: i32,
    n_green: i32,
}

impl CubeObservation {
    fn power(&self) -> i32 {
        return self.n_red * self.n_blue * self.n_green;
    }
}

impl FromStr for CubeObservation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut observation = CubeObservation { n_red: 0, n_blue: 0, n_green: 0};
        let mut comma_split = s.split(',');

        for color_indication in comma_split {
            let mut space_split = color_indication.trim().split(' ');
            let n_str = space_split.next().unwrap();
            let n = match n_str.parse::<i32>() {
                Ok(n) => n,
                Err(_) => return {
                    Err(format!("Invalid number of cubes: {} in {}", n_str, color_indication))
                },
            };
            let color_str = space_split.next().unwrap();
            match color_str {
                "red" => observation.n_red = n,
                "blue" => observation.n_blue = n,
                "green" => observation.n_green = n,
                _ => return Err(format!("Invalid color: {}", color_str)),
            }
        }

        Ok(observation)
    }
}
