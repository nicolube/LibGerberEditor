pub mod board;
pub mod error;
pub mod excellon_format;
pub mod gerber;
pub mod gerber_ascii;
pub mod layer;
pub mod unit_able;

use crate::layer::{LayerData, LayerType};
use derive_more::Display;
use serde::{Deserialize, Serialize};

pub type Result<T> = std::result::Result<T, error::Error>;

pub trait LayerCorners {
    fn get_size(&self) -> Size {
        let (min, max) = self.get_corners();
        let width = max.x - min.x;
        let height = max.y - min.y;
        Size { width, height }
    }

    fn get_corners(&self) -> (Pos, Pos);
}

pub trait LayerTransform {
    fn transform(&mut self, transform: &Pos);
}

pub trait LayerScale {
    fn scale(&mut self, x: f64, y: f64);
}

pub trait LayerMerge {
    fn merge(&mut self, other: &Self);
}

pub trait LayerStepAndRepeat {
    fn step_and_repeat(&mut self, x_repetitions: u32, y_repetitions: u32, offset: &Pos);
}

/// Position in mm
#[derive(Debug, Clone, PartialEq, Display, Serialize, Deserialize)]
#[display("x: {x:.2}, y: {y:.2}")]
pub struct Pos {
    pub x: f64,
    pub y: f64,
}

/// Size in mm
#[derive(Debug, Clone, PartialEq, Display, Serialize, Deserialize)]
#[display("width: {width:.2}, height: {height:.2}")]
pub struct Size {
    pub width: f64,
    pub height: f64,
}

#[macro_export]
macro_rules! load_layer_data {
    ($file:expr $(,)?) => {{
        let data = include_str!($file);
        let reader = std::io::BufReader::new(std::io::Cursor::new(data));
        let ty = LayerType::try_from($file.to_string().rsplitn(2, ".").next().unwrap()).unwrap();
        LayerData::parse(ty, reader).unwrap()
    }};
}

#[macro_export]
macro_rules! load_board_data {
    ($path:expr, $(($name:literal, $ty:expr)),* $(,)?) => {{
        Board(HashMap::from([
            $(
                ($ty, load_layer_data!(concat!($path, $name))),
            )*
        ]))
    }};
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use crate::board::Board;
    use super::*;

    #[test]
    fn it_works() {
        let folders = ["mobo"];
        if Path::new("output").exists() {
            fs::remove_dir_all("output").unwrap();
        }
        for folder in folders {
            let in_path = Path::new("test").join(folder);
            let out_path = Path::new("output").join(folder);
            fs::create_dir_all(&out_path).unwrap();
            println!("Processing folder: {:?}", in_path);
            let mut board = Board::from_folder(&in_path).unwrap();

            let (min, max) = board.get_corners();
            println!(
                "Transformed Corners: ({}, {}) - ({}, {})",
                min.x, min.y, max.x, max.y
            );

            let size = board.get_size();

            board.transform(&Pos {
                x: 100.0,
                y: -100.0,
            });

            board.transform(&Pos {
                x: -100.0,
                y: 100.0,
            });

            let mut copy = board.clone();
            copy.transform(&Pos {
                y: size.height + 5.0,
                x: 0.0,
            });
            board.merge(&copy);

            board.write_to_folder(&out_path).unwrap();
        }
    }
}
