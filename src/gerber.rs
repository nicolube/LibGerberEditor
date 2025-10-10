use crate::{
    LayerCorners, LayerData, LayerMerge, LayerScale, LayerStepAndRepeat, LayerTransform, LayerType,
    Pos,
};
use chrono::Utc;
use gerber_parser::gerber_types::{Aperture, ApertureDefinition, ApertureMacro, Command, CommentContent, CoordinateFormat, CoordinateMode, CoordinateNumber, Coordinates, DCode, ExtendedCode, FileAttribute, FunctionCode, GCode, GerberCode, GerberDate, GerberResult, ImageName, MCode, MacroContent, Operation, Polarity, QuadrantMode, StandardComment, StepAndRepeat, Unit, ZeroOmission};
use gerber_parser::{GerberDoc, ParseError};
use std::collections::HashMap;
use std::io::{BufReader, BufWriter, Read, Write};
use crate::unit_able::UnitAble;

#[derive(Debug, Clone, PartialEq)]
pub struct GerberLayerData {
    pub layer_type: LayerType,
    pub coordinate_format: CoordinateFormat,
    pub commands: Vec<Command>,
    unit: Unit,
    pub macros: HashMap<String, Vec<MacroContent>>,
    pub apertures: HashMap<i32, Aperture>,
}

impl GerberLayerData {
    pub fn new(ty: LayerType, data: GerberDoc) -> Result<Self, ParseError> {
        for command in &data.commands {
            if let Err(err) = command {
                eprintln!("Error parsing command in file: {}", err);
            }
        }

        let file_unit = data.units.unwrap_or(Unit::Millimeters);
        let mut unit = file_unit.clone();

        let macros = data
            .commands()
            .into_iter()
            .filter_map(|cmd|
                match cmd {
                    Command::ExtendedCode(ExtendedCode::ApertureMacro(mac)) => {
                        Some((mac.name.clone(), mac.content.clone()))
                    }
                    Command::FunctionCode(FunctionCode::GCode(GCode::Unit(cmd))) => {
                        unit = cmd.clone();
                        None
                    }
                    _ => None,
                })
            .collect::<HashMap<_, _>>();


        let commands = data
            .commands()
            .into_iter()
            .skip_while(|cmd| {
                !matches!(cmd, Command::FunctionCode(FunctionCode::DCode(_)))
                    && !matches!(
                        cmd,
                        Command::FunctionCode(FunctionCode::GCode(GCode::RegionMode(_)))
                    )
            })
            .filter(|cmd| {
                !matches!(
                    cmd,
                    Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile))
                )
            })
            .cloned()
            .collect();

        Ok(Self {
            unit: data.units.unwrap_or(Unit::Millimeters),
            coordinate_format: data.format_specification.unwrap(),
            layer_type: ty,
            commands,
            macros,
            apertures: data.apertures,
        }.to_unit(&Unit::Millimeters))
    }

    pub fn from_type<R>(ty: LayerType, reader: BufReader<R>) -> Result<Self, ParseError>
    where
        R: Read,
    {
        let data = gerber_parser::parse(reader).map_err(|(_, err)| err)?;
        Self::new(ty, data)
    }

    pub fn from_commands<R>(reader: BufReader<R>) -> Result<Self, ParseError>
    where
        R: Read,
    {
        let data = gerber_parser::parse(reader).map_err(|(_, err)| err)?;

        let layer_type = LayerType::try_from(&data.commands())
            .map_err(|e| ParseError::IoError(format!("Failed to find type: {}", e)))?;

        Self::new(layer_type, data)
    }

    pub fn empty(layer_type: LayerType) -> Self {
        Self {
            coordinate_format: CoordinateFormat::new(ZeroOmission::Leading, CoordinateMode::Absolute, 4, 6),
            apertures: HashMap::new(),
            layer_type,
            macros: HashMap::new(),
            unit: Unit::Millimeters,
            commands: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }

    pub fn to_unit(mut self, unit: &Unit) -> Self {
        if unit == &self.unit {
            return self;
        }

        self.coordinate_format = CoordinateFormat::new(ZeroOmission::Leading, CoordinateMode::Absolute, 4, 6);

        match unit {
            Unit::Millimeters => self.scale(25.4, 25.4),
            Unit::Inches => self.scale(1.0 / 25.4, 1.0 / 25.4),
        }

        for (_, aperture) in &mut self.apertures {
            match aperture {
                Aperture::Circle(circle) => {
                    circle.diameter.convert_unit_self(&self.unit, unit);
                    circle.hole_diameter.convert_unit_self(&self.unit, unit);
                }
                Aperture::Obround(rect) | Aperture::Rectangle(rect) => {
                    rect.x.convert_unit_self(&self.unit, unit);
                    rect.y.convert_unit_self(&self.unit, unit);
                    rect.hole_diameter.convert_unit_self(&self.unit, unit);
                }
                Aperture::Polygon(poly) => {
                    poly.diameter.convert_unit_self(&self.unit, unit);
                    poly.hole_diameter.convert_unit_self(&self.unit, unit);
                }
                // TODO: whatever we need to do here.
                Aperture::Macro(_, Some(_)) => {}
                Aperture::Macro(_, None) => {}
            };
        }

       self.macros.values_mut().for_each(|contents| {
            contents.iter_mut().for_each(|content| {
                content.convert_unit_self( &self.unit, unit);
            });
        });

        self.unit = unit.clone();
        self
    }

    fn to_commands(&self) -> Vec<Command> {
        let mut commands = vec![
            Command::FunctionCode(FunctionCode::GCode(GCode::Comment(CommentContent::String(
                "ProAdm generated panel".to_string(),
            )))),
        ];
        commands.extend(
            [
                FileAttribute::FileFunction(self.layer_type.function()),
                FileAttribute::CreationDate(GerberDate::from(Utc::now())),
            ]
                .into_iter()
                .map(|fa| {
                    Command::FunctionCode(FunctionCode::GCode(GCode::Comment(
                        CommentContent::Standard(StandardComment::FileAttribute(fa)),
                    )))
                }),
        );
        commands.extend([
            Command::FunctionCode(FunctionCode::GCode(GCode::QuadrantMode(
                QuadrantMode::Multi,
            ))),
            Command::ExtendedCode(ExtendedCode::Unit(self.unit.clone())),
            Command::ExtendedCode(ExtendedCode::CoordinateFormat(
                self.coordinate_format.clone(),
            )),
            Command::ExtendedCode(ExtendedCode::LoadPolarity(Polarity::Dark)),
            Command::ExtendedCode(ExtendedCode::ImageName(ImageName {
                name: self.layer_type.file_ending(),
            })),
        ]);

        commands.extend(
            self.apertures
                .iter()
                .filter(|(_, current)| !matches!(current, Aperture::Macro(..)))
                .map(|(id, aperture)| {
                    Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: *id,
                        aperture: aperture.clone(),
                    }))
                }),
        );
        commands.extend(
            self.macros
                .iter()
                .map(|(name, content)| ApertureMacro {
                    name: name.clone(),
                    content: content.clone(),
                })
                .map(|x| Command::ExtendedCode(ExtendedCode::ApertureMacro(x.clone()))),
        );
        commands.extend(
            self.apertures
                .iter()
                .filter(|(_, current)| matches!(current, Aperture::Macro(..)))
                .map(|(id, aperture)| {
                    Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: *id,
                        aperture: aperture.clone(),
                    }))
                }),
        );
        commands.extend(self.commands.iter().cloned());
        commands.push(Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile)));
        commands
    }

    pub fn write_to<T>(&self, writer: &mut BufWriter<T>) -> GerberResult<()>
    where
        T: Write,
    {
        self.to_commands().serialize(writer)?;
        writer.flush()?;
        Ok(())
    }
}

fn min_max(min: &mut Pos, max: &mut Pos, coords: &Coordinates, unit: &Unit) {
    if let Some(x) = coords.x {
        let x = x.to_mm(unit).into();
        if x < min.x {
            min.x = x;
        }
        if x > max.x {
            max.x = x;
        }
    }
    if let Some(y) = coords.y {
        let y = y.to_mm(unit).into();
        if y < min.y {
            min.y = y;
        }
        if y > max.y {
            max.y = y;
        }
    }
}

impl LayerCorners for GerberLayerData {
    fn get_corners(&self) -> (Pos, Pos) {
        (&self.unit, &self.commands).get_corners()
    }
}

impl LayerTransform for GerberLayerData {
    fn transform(&mut self, pos: &Pos) {
        (&self.unit, &mut self.commands).transform(pos);
    }
}

impl LayerScale for GerberLayerData {
    fn scale(&mut self, x: f64, y: f64) {
        (&self.coordinate_format, &mut self.commands).scale(x, y);
    }
}

impl LayerMerge for GerberLayerData {
    fn merge(&mut self, other: &Self) {
        let mut aperture_id = 10;
        let mut aperture_id_map = HashMap::new();

        let other = other.clone().to_unit(&self.unit);

        for (id, aperture) in &other.apertures {
            let found_id =
                self.apertures.iter().find_map(
                    |(id, current)| {
                        if current == aperture { Some(id) } else { None }
                    },
                );
            while self.apertures.get(&aperture_id).is_some() && found_id.is_none() {
                aperture_id += 1;
            }

            let aperture_id = *found_id.unwrap_or(&aperture_id);
            aperture_id_map.insert(id, aperture_id);
            self.apertures.insert(aperture_id, aperture.clone());
        }

        for command in other.commands {
            if let Command::FunctionCode(FunctionCode::DCode(DCode::SelectAperture(id))) = command {
                let d_code = aperture_id_map.get(&id).unwrap();
                self.commands
                    .push(Command::FunctionCode(FunctionCode::DCode(
                        DCode::SelectAperture(*d_code),
                    )));
            } else if let Command::FunctionCode(FunctionCode::DCode(DCode::Operation(op))) = command
            {
                let mut op = op.clone();
                match &mut op {
                    Operation::Interpolate(Some(cords), _)
                    | Operation::Move(Some(cords))
                    | Operation::Flash(Some(cords)) => {
                        cords.format = self.coordinate_format.clone();
                    }
                    _ => {}
                };
                if let Operation::Interpolate(_, Some(cords)) = &mut op {
                    cords.format = self.coordinate_format.clone()
                }
                self.commands
                    .push(Command::FunctionCode(FunctionCode::DCode(
                        DCode::Operation(op),
                    )));
            } else {
                self.commands.push(command.clone());
            }
        }
    }
}

impl LayerStepAndRepeat for GerberLayerData {
    fn step_and_repeat(&mut self, x_repetitions: u32, y_repetitions: u32, offset: &Pos) {
        (&self.unit, &mut self.commands).step_and_repeat(x_repetitions, y_repetitions, offset);
    }
}

impl LayerCorners for (&Unit, &Vec<Command>) {
    fn get_corners(&self) -> (Pos, Pos) {
        let (unit, cmds) = self;
        let mut min = Pos {
            x: f64::MAX,
            y: f64::MAX,
        };
        let mut max = Pos {
            x: f64::MIN,
            y: f64::MIN,
        };
        for command in cmds.iter() {
            if let Command::FunctionCode(FunctionCode::DCode(DCode::Operation(op))) = command {
                match op {
                    Operation::Move(Some(coords))
                    | Operation::Flash(Some(coords))
                    | Operation::Interpolate(Some(coords), _) => {
                        min_max(&mut min, &mut max, coords, unit);
                    }
                    _ => {}
                }
            }
        }
        (min, max)
    }
}

impl LayerTransform for (&Unit, &mut Vec<Command>) {
    fn transform(&mut self, transform: &Pos) {
        let (unit, cmds) = self;
        let to_unit = |x: f64| CoordinateNumber::try_from(x.mm_to_unit(unit)).ok();
        let to_mm = |x: &CoordinateNumber| f64::from(*x).to_mm(unit);
        for command in cmds.iter_mut() {
            if let Command::FunctionCode(FunctionCode::DCode(DCode::Operation(op))) = command {
                match op {
                    Operation::Move(Some(coords))
                    | Operation::Flash(Some(coords))
                    | Operation::Interpolate(Some(coords), _) => {
                        if let Some(x) = coords.x {
                            coords.x = to_unit(to_mm(&x) + transform.x);
                        }
                        if let Some(y) = coords.y {
                            coords.y = to_unit(to_mm(&y) + transform.y);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

impl LayerScale for (&CoordinateFormat, &mut Vec<Command>) {
    fn scale(&mut self, x: f64, y: f64) {
        let (format, cmds) = self;
        for cmd in cmds.iter_mut() {
            if let Command::FunctionCode(FunctionCode::DCode(DCode::Operation(op))) = cmd {
                match op {
                    Operation::Interpolate(Some(pos), _)
                    | Operation::Move(Some(pos))
                    | Operation::Flash(Some(pos)) => {
                        pos.x = pos
                            .x
                            .map(|v| CoordinateNumber::try_from(f64::from(v) * x).unwrap());
                        pos.y = pos
                            .y
                            .map(|v| CoordinateNumber::try_from(f64::from(v) * y).unwrap());
                        pos.format = format.clone();
                    }
                    _ => {}
                }
                if let Operation::Interpolate(_, Some(pos)) = op {
                    pos.x = pos
                        .x
                        .map(|v| CoordinateNumber::try_from(f64::from(v) * x).unwrap());
                    pos.y = pos
                        .y
                        .map(|v| CoordinateNumber::try_from(f64::from(v) * y).unwrap());
                    pos.format = format.clone();
                }
            }
        }
    }
}

impl LayerStepAndRepeat for (&Unit, &mut Vec<Command>) {
    fn step_and_repeat(&mut self, x_repetitions: u32, y_repetitions: u32, offset: &Pos) {
        let sr = Command::ExtendedCode(ExtendedCode::StepAndRepeat(StepAndRepeat::Open {
            repeat_x: x_repetitions,
            repeat_y: y_repetitions,
            distance_x: offset.x.mm_to_unit(self.0),
            distance_y: offset.y.mm_to_unit(self.0),
        }));
        let commands = &mut self.1;
        commands.insert(0, sr);
        commands.push(Command::ExtendedCode(ExtendedCode::StepAndRepeat(
            StepAndRepeat::Close,
        )))
    }
}

impl From<GerberLayerData> for LayerData {
    fn from(value: GerberLayerData) -> Self {
        LayerData::Gerber(value)
    }
}
