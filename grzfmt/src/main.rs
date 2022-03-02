use std::io::{stdin, stdout, BufWriter, Read, Write};

use clap::Arg;
use grezi::{
    layout::{self, Constraint},
    Token,
};

enum InputType {
    Map(memmap::Mmap),
    Stdin(Vec<u8>),
}

impl AsRef<[u8]> for InputType {
    fn as_ref(&self) -> &[u8] {
        match *self {
            Self::Map(ref map) => map.as_ref(),
            Self::Stdin(ref vec) => vec.as_ref(),
        }
    }
}

impl InputType {
    fn init(&mut self) -> Result<usize, std::io::Error> {
        match self {
            Self::Map(_) => Ok(0),
            Self::Stdin(vec) => stdin().read_to_end(vec),
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    let args = clap::Command::new("GRZFmt").arg(Arg::new("INPUT").required(true));
    let mut map = unsafe {
        match args.get_matches().value_of("INPUT").unwrap_unchecked() {
            "-" => InputType::Stdin(Vec::new()),
            s => InputType::Map(memmap::Mmap::map(&std::fs::File::open(s)?)?),
        }
    };
    map.init()?;
    let tokens = grezi::tokenizer(map.as_ref());
    if tokens.1.is_empty() {
        let mut writer = BufWriter::new(stdout());
        for i in unsafe { tokens.0.unwrap_unchecked() } {
            match i {
                Token::Obj(((name, type_), values), _) => {
                    writer.write_fmt(format_args!("{}: {}(\n", name, type_))?;
                    for j in values {
                        writer.write_fmt(format_args!("\t{}: \"{}\",\n", j.0, j.1))?;
                    }
                    writer.write_all(b");")?;
                }
                Token::Slide(arg, _) => {
                    writer.write_all(b"{\n")?;
                    for j in arg {
                        writer.write_fmt(format_args!(
                            "\t{}: {}[{}]{}{}{}{},\n",
                            j.0 .0 .0,
                            j.0 .0 .1,
                            j.0 .1,
                            j.1 .0 .0,
                            j.1 .0 .1,
                            j.1 .1 .0,
                            j.1 .1 .1
                        ))?;
                    }
                    writer.write_all(b"};")?;
                }
                Token::Viewbox(((((name, split), index), direction), constraints), _) => {
                    writer.write_fmt(format_args!(
                        "{}: {}[{}] {}\n",
                        name,
                        split,
                        index,
                        match direction {
                            layout::Direction::Vertical => "^",
                            layout::Direction::Horizontal => ">",
                        }
                    ))?;
                    for j in constraints {
                        match j {
                            Constraint::Max(n) => writer.write_fmt(format_args!("\t{}+,\n", n))?,
                            Constraint::Min(n) => writer.write_fmt(format_args!("\t{}-,\n", n))?,
                            Constraint::Ratio(n, m) => {
                                writer.write_fmt(format_args!("\t{}:{},\n", n, m))?
                            }
                            Constraint::Length(n) => {
                                writer.write_fmt(format_args!("\t{},\n", n))?
                            }
                            Constraint::Percentage(n) => {
                                writer.write_fmt(format_args!("\t{}%,\n", n))?
                            }
                        };
                    }
                    writer.write_all(b"];")?;
                }
                Token::Register((name, val)) => {
                    if val.split_whitespace().next().is_some() {
                        writer.write_fmt(format_args!(
                            "{}: \"{}\";",
                            name.to_ascii_uppercase(),
                            val
                        ))?;
                    } else {
                        writer.write_fmt(format_args!(
                            "{}: {};",
                            name.to_ascii_uppercase(),
                            val
                        ))?;
                    }
                }
                Token::Command(((name, field), value), _) => {
                    writer.write_fmt(format_args!("{}.{}: \"{}\";", name, field, value))?;
                }
            }
            writer.write_all(b"\n\n")?;
        }
        writer.flush()
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Failed to compile presentation",
        ))
    }
}
