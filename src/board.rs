use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::Path;
use gerber_parser::gerber_types::{Command, CommentContent, FunctionCode, GCode, GerberResult};
use crate::{error, excellon_format, LayerCorners, LayerMerge, LayerTransform, Pos};
use crate::layer::{Layer, LayerData, LayerType};

#[derive(Debug, Clone, PartialEq)]
pub struct Board(Vec<Layer>);

impl Board {
    pub fn new(data: Vec<(&str, BufReader<&mut dyn Read>)>) -> crate::Result<Self> {
        let mut result = Vec::new();
        for (name, reader) in data {
            let ty = LayerType::try_from(name.rsplitn(2, ".").next().unwrap());
            match ty {
                Ok(ty) => {
                    let (ty, data) = LayerData::parse(ty, reader)
                        .map_err(|err| error::Error::ParseError(err, name.to_string()))?;
                    result.push(Layer {
                        ty,
                        name: name.to_string(),
                        data,
                    })
                }
                Err(_) => return Err(error::Error::InvalidType(name.to_string())),
            }
        }
        Ok(Self(result))
    }

    pub fn comment(&mut self, txt: String) {
        for layer in self.0.iter_mut() {
            match &mut layer.data {
                LayerData::Gerber(g) => {
                    g.commands
                        .push(Command::FunctionCode(FunctionCode::GCode(GCode::Comment(
                            CommentContent::String(txt.clone()),
                        ))))
                }
                LayerData::Excellon(e) => e
                    .commands
                    .push(Ok(excellon_format::Command::Comment(txt.clone()))),
                LayerData::Info(_) => {}
            }
        }
    }

    pub fn from_folder(path: &Path) -> crate::Result<Self> {
        let folder = fs::read_dir(path)?;
        let mut files = folder
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| {
                let name = entry.file_name().to_string_lossy().to_string();
                let ty = LayerType::try_from(name.rsplitn(2, ".").next().unwrap());
                if matches!(entry.file_type(), Ok(ty) if ty.is_file()) && ty.is_ok() {
                    Some(
                        File::open(entry.path().as_path())
                            .map(|f| (name, f))
                            .map_err(|e| e.into()),
                    )
                } else {
                    None
                }
            })
            .collect::<crate::Result<Vec<_>>>()?;
        let reader = files
            .iter_mut()
            .map(|(name, file)| {
                let reader: &mut dyn Read = file;
                (name.as_str(), BufReader::new(reader))
            })
            .collect::<Vec<_>>();
        Self::new(reader)
    }
    pub fn write_to<T>(
        &self,
        f: &mut impl FnMut(&Layer) -> std::io::Result<BufWriter<T>>,
    ) -> GerberResult<()>
    where
        T: Write,
    {
        for layer in &self.0 {
            let mut writer = f(layer)?;
            layer.data.write_to(&mut writer)?;
        }
        Ok(())
    }

    pub fn layers(&self) -> Vec<&Layer> {
        self.0.iter().collect()
    }

    pub fn get_layer(&self, ty: &LayerType) -> Option<&Layer> {
        self.0.iter().find(|layer| &layer.ty == ty)
    }

    pub fn get_layer_mut(&mut self, ty: &LayerType) -> Option<&mut Layer> {
        self.0.iter_mut().find(|layer| &layer.ty == ty)
    }

    pub fn write_to_folder(&self, path: &Path) -> GerberResult<()> {
        fs::create_dir_all(path)?;
        let mut name_fn = |x: &Layer| {
            let file_path = path.join(&x.name);
            Ok(BufWriter::new(File::create(file_path)?))
        };
        self.write_to(&mut name_fn)
    }
}

impl LayerCorners for Board {
    fn get_corners(&self) -> (Pos, Pos) {
        let mut min = Pos {
            x: f64::MAX,
            y: f64::MAX,
        };
        let mut max = Pos {
            x: f64::MIN,
            y: f64::MIN,
        };
        for layer in self.0.iter() {
            if let LayerData::Gerber(layer) = &layer.data {
                let (layer_min, layer_max) = layer.get_corners();
                if layer_min.x < min.x {
                    min.x = layer_min.x;
                }
                if layer_max.x > max.x {
                    max.x = layer_max.x;
                }
                if layer_min.y < min.y {
                    min.y = layer_min.y;
                }
                if layer_max.y > max.y {
                    max.y = layer_max.y;
                }
            }
        }
        (min, max)
    }
}

impl LayerTransform for Board {
    fn transform(&mut self, transform: &Pos) {
        for layer in &mut self.0 {
            layer.data.transform(transform);
        }
    }
}

impl LayerMerge for Board {
    fn merge(&mut self, other: &Self) {
        for layer in &mut self.0 {
            other
                .get_layer(&layer.ty)
                .map(|other| layer.data.merge(&other.data));
        }
    }
}