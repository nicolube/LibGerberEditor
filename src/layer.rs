use crate::error::ParseError;
use crate::excellon_format::{ExcellonLayerData, parse_excellon};
use crate::gerber::GerberLayerData;
use crate::{LayerMerge, LayerStepAndRepeat, LayerTransform, Pos};
use gerber_parser::gerber_types::{
    Command, CommentContent, ExtendedCode, ExtendedPosition, FileAttribute, FileFunction,
    FunctionCode, GCode, GerberResult, Position, Profile, StandardComment,
};
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::io::{BufReader, BufWriter, Read, Write};
use std::result;

#[derive(Debug, Clone, PartialEq)]
pub enum LayerData {
    Gerber(GerberLayerData),
    Excellon(ExcellonLayerData),
    Info(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layer {
    pub ty: LayerType,
    pub name: String,
    pub data: LayerData,
}

impl LayerData {
    pub fn parse<T>(
        ty: LayerType,
        reader: BufReader<T>,
    ) -> result::Result<(LayerType, LayerData), ParseError>
    where
        T: Read,
    {
        Ok(match ty {
            LayerType::Drill => (
                ty.clone(),
                LayerData::Excellon(
                    parse_excellon(reader).map_err(|e| ParseError::ExcellonParseError(e))?,
                ),
            ),
            LayerType::UndefinedGerber => {
                let layer = GerberLayerData::from_commands(reader)?;
                (layer.layer_type.clone(), LayerData::Gerber(layer))
            }
            _ => (
                ty.clone(),
                LayerData::Gerber(GerberLayerData::from_type(ty, reader)?),
            ),
        })
    }

    pub fn write_to<T>(&self, writer: &mut BufWriter<T>) -> GerberResult<()>
    where
        T: Write,
    {
        match self {
            LayerData::Gerber(g) => g.write_to(writer)?,
            LayerData::Excellon(e) => e.write_to(writer)?,
            LayerData::Info(s) => writer.write_all(s.to_string().as_bytes())?,
        }
        Ok(())
    }

    pub fn get_type(&self) -> LayerType {
        match self {
            LayerData::Gerber(layer) => layer.layer_type.clone(),
            LayerData::Excellon(_) => LayerType::Drill,
            LayerData::Info(_) => LayerType::Info,
        }
    }
}

impl LayerMerge for LayerData {
    fn merge(&mut self, other: &Self) {
        match (self, other) {
            (LayerData::Excellon(s), LayerData::Excellon(o)) => {
                s.merge(o);
            }
            (LayerData::Gerber(s), LayerData::Gerber(o)) => {
                s.merge(o);
            }
            _ => panic!("Cannot merge layers of diffrent type"),
        }
    }
}

impl LayerTransform for LayerData {
    fn transform(&mut self, transform: &Pos) {
        match self {
            LayerData::Excellon(s) => s.transform(transform),
            LayerData::Gerber(s) => s.transform(transform),
            LayerData::Info(_) => {}
        }
    }
}

impl LayerStepAndRepeat for LayerData {
    fn step_and_repeat(&mut self, x_repetitions: u32, y_repetitions: u32, offset: &Pos) {
        match self {
            LayerData::Gerber(g) => {
                g.step_and_repeat(x_repetitions, y_repetitions, offset);
            }
            LayerData::Excellon(e) => {
                e.step_and_repeat(x_repetitions, y_repetitions, offset);
            }
            LayerData::Info(_) => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LayerType {
    Top,
    Bottom,
    Inner(i32),
    PasteTop,
    PasteBottom,
    MaskTop,
    MaskBottom,
    SilkScreenTop,
    SilkScreenBottom,
    Drill,
    Dimensions,
    Milling,
    VCut,
    SidePlating,
    KeepOut,
    Info,
    UndefinedGerber,
}

impl TryFrom<&str> for LayerType {
    type Error = String;

    fn try_from(value: &str) -> result::Result<Self, Self::Error> {
        match value.to_uppercase().as_str() {
            "GTL" => Ok(LayerType::Top),
            "GBL" => Ok(LayerType::Bottom),
            "GTP" => Ok(LayerType::PasteTop),
            "GBP" => Ok(LayerType::PasteBottom),
            "GTS" => Ok(LayerType::MaskTop),
            "GBS" => Ok(LayerType::MaskBottom),
            "GTO" => Ok(LayerType::SilkScreenTop),
            "GBO" => Ok(LayerType::SilkScreenBottom),
            "DRD" | "DRL" => Ok(LayerType::Drill),
            "GM1" => Ok(LayerType::Dimensions),
            "GM2" => Ok(LayerType::Milling),
            "GVC" => Ok(LayerType::VCut),
            "GSP" => Ok(LayerType::SidePlating),
            "GKO" => Ok(LayerType::KeepOut),
            "GBR" => Ok(LayerType::UndefinedGerber),
            _ if value.starts_with("GL") => {
                let inner_num = value[2..]
                    .parse::<i32>()
                    .map_err(|_| "GL must be followed by numbers")?;
                Ok(LayerType::Inner(inner_num))
            }
            _ => Err(format!("Invalid layer type: {}", value)),
        }
    }
}

impl TryFrom<&Vec<&Command>> for LayerType {
    type Error = String;

    fn try_from(value: &Vec<&Command>) -> result::Result<Self, Self::Error> {
        let command = value
            .iter()
            .find_map(|c| match c {
                Command::ExtendedCode(ExtendedCode::FileAttribute(
                    FileAttribute::FileFunction(file_function),
                )) => Some(file_function),
                Command::FunctionCode(FunctionCode::GCode(GCode::Comment(
                    CommentContent::Standard(StandardComment::FileAttribute(
                        FileAttribute::FileFunction(file_function),
                    )),
                ))) => Some(file_function),
                _ => None,
            })
            .ok_or("No file function found")?;
        Ok(LayerType::layer_type(command))
    }
}

impl LayerType {
    pub fn file_ending(&self) -> String {
        match self {
            LayerType::Info => "txt".to_string(),
            LayerType::Drill => "drl".to_string(),
            LayerType::UndefinedGerber => "gbr".to_string(),
            _ => "gbr".to_string(),
        }
    }

    pub fn layer_type(file_function: &FileFunction) -> LayerType {
        match file_function {
            FileFunction::Copper {
                layer: _,
                pos: ExtendedPosition::Top,
                copper_type: None,
            } => LayerType::Top,
            FileFunction::Copper {
                layer: _,
                pos: ExtendedPosition::Bottom,
                copper_type: None,
            } => LayerType::Bottom,
            FileFunction::Copper {
                layer,
                pos: ExtendedPosition::Inner,
                copper_type: None,
            } => LayerType::Inner(*layer),
            FileFunction::Paste(Position::Top) => LayerType::PasteTop,
            FileFunction::Paste(Position::Bottom) => LayerType::PasteBottom,
            FileFunction::SolderMask {
                pos: Position::Top,
                index: None,
            } => LayerType::MaskTop,
            FileFunction::SolderMask {
                pos: Position::Bottom,
                index: None,
            } => LayerType::MaskBottom,
            FileFunction::Legend {
                pos: Position::Top,
                index: None,
            } => LayerType::SilkScreenTop,
            FileFunction::Legend {
                pos: Position::Bottom,
                index: None,
            } => LayerType::SilkScreenBottom,
            FileFunction::DrillMap => LayerType::Drill,
            FileFunction::Profile(Some(Profile::NonPlated)) => LayerType::Dimensions,
            FileFunction::VCut(None) => LayerType::VCut,
            FileFunction::Profile(Some(Profile::Plated)) => LayerType::SidePlating,
            FileFunction::KeepOut(Position::Top) => LayerType::KeepOut,
            FileFunction::Other(_) => LayerType::Info,
            _ => LayerType::UndefinedGerber,
        }
    }

    pub fn function(&self) -> FileFunction {
        match self {
            LayerType::Top => FileFunction::Copper {
                layer: 1,
                pos: ExtendedPosition::Top,
                copper_type: None,
            },
            LayerType::Bottom => FileFunction::Copper {
                layer: 99,
                pos: ExtendedPosition::Bottom,
                copper_type: None,
            },
            LayerType::Inner(layer) => FileFunction::Copper {
                layer: *layer,
                pos: ExtendedPosition::Inner,
                copper_type: None,
            },
            LayerType::PasteTop => FileFunction::Paste(Position::Top),
            LayerType::PasteBottom => FileFunction::Paste(Position::Bottom),
            LayerType::MaskTop => FileFunction::SolderMask {
                pos: Position::Top,
                index: None,
            },
            LayerType::MaskBottom => FileFunction::SolderMask {
                pos: Position::Bottom,
                index: None,
            },
            LayerType::SilkScreenTop => FileFunction::Legend {
                pos: Position::Top,
                index: None,
            },
            LayerType::SilkScreenBottom => FileFunction::Legend {
                pos: Position::Bottom,
                index: None,
            },
            LayerType::Drill => FileFunction::DrillMap,
            LayerType::Dimensions => FileFunction::Profile(None),
            LayerType::Milling => FileFunction::Profile(Some(Profile::NonPlated)),
            LayerType::VCut => FileFunction::VCut(None),
            LayerType::SidePlating => FileFunction::Profile(Some(Profile::Plated)),
            LayerType::KeepOut => FileFunction::KeepOut(Position::Top),
            LayerType::Info => FileFunction::Other(String::from("Text")),
            LayerType::UndefinedGerber => FileFunction::Other(String::from("Undefined")),
        }
    }
}

impl Display for LayerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.file_ending())?;
        match self {
            LayerType::Top => write!(f, "Copper Top Layer")?,
            LayerType::Bottom => write!(f, "Copper Bottom Layer")?,
            LayerType::Inner(num) => write!(f, "Copper Inner Layer {}", num)?,
            LayerType::PasteTop => write!(f, "Paste Top Layer")?,
            LayerType::PasteBottom => write!(f, "Paste Bottom Layer")?,
            LayerType::MaskTop => write!(f, "Mask Top Layer")?,
            LayerType::MaskBottom => write!(f, "Mask Bottom Layer")?,
            LayerType::SilkScreenTop => write!(f, "Silk Screen Top Layer")?,
            LayerType::SilkScreenBottom => write!(f, "Silk Screen Bottom Layer")?,
            LayerType::Drill => write!(f, "Drill Layer")?,
            LayerType::Dimensions => write!(f, "Dimension Layer")?,
            LayerType::Milling => write!(f, "Milling Layer")?,
            LayerType::VCut => write!(f, "V-Cut Layer")?,
            LayerType::SidePlating => write!(f, "Side Plating Layer")?,
            LayerType::KeepOut => write!(f, "Keep Out Layer")?,
            LayerType::Info => write!(f, "Info")?,
            LayerType::UndefinedGerber => write!(f, "Undefined")?,
        };
        Ok(())
    }
}


impl From<Layer> for LayerData {
    fn from(layer: Layer) -> Self {
        layer.data
    }
}

impl From<&Layer> for LayerData {
    fn from(layer: &Layer) -> Self {
        layer.data.clone()
    }
}