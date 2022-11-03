use std::fmt::{Display, Formatter};

///
/// Error type
///
#[derive(Debug)]
#[allow(missing_docs)]
pub enum OFError {
    Ods(String),
    Io(std::io::Error),
    Parse(String),
    ParseInt(std::num::ParseIntError),
    ParseBool(std::str::ParseBoolError),
    ParseFloat(std::num::ParseFloatError),
    ParseExpr(crate::parse2::ParseExprError),
    Chrono(chrono::format::ParseError),
    Duration(time::OutOfRangeError),
    SystemTime(std::time::SystemTimeError),
    Nom(nom::Err<nom::error::Error<String>>),
}

impl Display for OFError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OFError::Ods(e) => write!(f, "Ods {}", e)?,
            OFError::Io(e) => write!(f, "IO {}", e)?,
            OFError::Parse(e) => write!(f, "Parse {}", e)?,
            OFError::ParseInt(e) => write!(f, "ParseInt {}", e)?,
            OFError::ParseBool(e) => write!(f, "ParseBool {}", e)?,
            OFError::ParseFloat(e) => write!(f, "ParseFloat {}", e)?,
            OFError::Chrono(e) => write!(f, "Chrono {}", e)?,
            OFError::Duration(e) => write!(f, "Duration {}", e)?,
            OFError::SystemTime(e) => write!(f, "SystemTime {}", e)?,
            OFError::Nom(e) => write!(f, "Nom {}", e)?,
            OFError::ParseExpr(e) => write!(f, "ParseExpr {}", e)?,
        }

        Ok(())
    }
}

impl std::error::Error for OFError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            OFError::Ods(_) => None,
            OFError::Io(e) => Some(e),
            OFError::Parse(_) => None,
            OFError::ParseInt(e) => Some(e),
            OFError::ParseBool(e) => Some(e),
            OFError::ParseFloat(e) => Some(e),
            OFError::Chrono(e) => Some(e),
            OFError::Duration(e) => Some(e),
            OFError::SystemTime(e) => Some(e),
            OFError::Nom(e) => Some(e),
            OFError::ParseExpr(e) => Some(e),
        }
    }
}

impl From<time::OutOfRangeError> for OFError {
    fn from(err: time::OutOfRangeError) -> OFError {
        OFError::Duration(err)
    }
}

impl From<std::io::Error> for OFError {
    fn from(err: std::io::Error) -> OFError {
        OFError::Io(err)
    }
}

impl From<std::str::ParseBoolError> for OFError {
    fn from(err: std::str::ParseBoolError) -> OFError {
        OFError::ParseBool(err)
    }
}

impl From<std::num::ParseIntError> for OFError {
    fn from(err: std::num::ParseIntError) -> OFError {
        OFError::ParseInt(err)
    }
}

impl From<crate::parse2::ParseExprError> for OFError {
    fn from(err: crate::parse2::ParseExprError) -> OFError {
        OFError::ParseExpr(err)
    }
}

impl From<std::num::ParseFloatError> for OFError {
    fn from(err: std::num::ParseFloatError) -> OFError {
        OFError::ParseFloat(err)
    }
}

impl From<chrono::format::ParseError> for OFError {
    fn from(err: chrono::format::ParseError) -> OFError {
        OFError::Chrono(err)
    }
}

impl From<std::time::SystemTimeError> for OFError {
    fn from(err: std::time::SystemTimeError) -> OFError {
        OFError::SystemTime(err)
    }
}

impl<I> From<nom::Err<nom::error::Error<I>>> for OFError
where
    I: ToString,
{
    fn from(err: nom::Err<nom::error::Error<I>>) -> OFError {
        OFError::Nom(
            err.map(|e| nom::error::ParseError::from_error_kind(e.input.to_string(), e.code)),
        )
    }
}
