use std::fmt::{self, Formatter};

pub type Result = std::fmt::Result;

/// Wrapper for writing scad cleanly
///
/// Instead of:
/// ```
/// use scad::refactor::*;
/// use std::fmt::Formatter;
///
/// enum Sphere {};
///
/// impl Scad for Sphere {
///     fn write_scad(&self, f: &mut Formatter) -> Result {
///         let radius = 2.5;
///         write!(f, "sphere(r=")?;
///         radius.write_scad(f)?;
///         write!(f, ")")
///     }
/// }
/// ```
///
/// You can just write:
///
/// ```
/// use scad::refactor::*;
/// use std::fmt::Formatter;
///
/// enum Sphere {};
///
/// impl Scad for Sphere {
///     fn write_scad(&self, f: &mut Formatter) -> Result {
///         let radius = 2.5;
///         write!(f, "sphere(r={})", ToScad(&radius))
///     }
/// }
/// ```
pub struct ToScad<'a, T: ?Sized>(pub &'a T);

impl<'a, T: Scad + ?Sized> fmt::Display for ToScad<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.write_scad(f)
    }
}

pub trait Scad {
    fn write_scad(&self, f: &mut Formatter) -> Result;
    fn to_scad(&self) -> String {
        ToScad(self).to_string()
    }
}

#[duplicate::duplicate(primitive; [f64]; [u32]; [&str])]
impl Scad for primitive {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

fn write_scad_iter<'a, T: Scad + 'a>(
    mut iter: impl Iterator<Item = &'a T>,
    f: &mut Formatter,
) -> Result {
    write!(f, "[")?;

    if let Some(elem) = iter.next() {
        elem.write_scad(f)?;
    }
    for elem in iter {
        write!(f, ",{}", ToScad(elem))?;
    }

    write!(f, "]")
}

impl<T: Scad> Scad for [T] {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write_scad_iter(self.iter(), f)
    }
}

impl<T: Scad> Scad for &[T] {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write_scad_iter(self.iter(), f)
    }
}

impl<T: Scad, const N: usize> Scad for [T; N] {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write_scad_iter(self.iter(), f)
    }
}

/// Since scad allows creation of circle like objects using either radius or diameter,
/// this enum specifies which format to use
#[derive(Clone, Copy, Debug)]
pub enum CircleSize {
    Radius(f64),
    Diameter(f64),
}

impl Scad for CircleSize {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use CircleSize::*;

        match self {
            Radius(v) | Diameter(v) => write!(f, "{}", v),
        }
    }
}

/// Scad allows using one number in place of a vector of 3 floats
/// with cubes for example
#[derive(Clone, Debug)]
pub enum CubeSize {
    Scalar(f64),
    Vector3([f64; 3]),
}

pub enum ThreeD<'a> {
    /// sphere(radius | d=diameter)
    ///
    /// Source: <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#sphere>
    Sphere(Option<CircleSize>),
    /// cube(size, center)
    ///
    /// Source <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#cube>
    Cube(Option<CubeSize>, Option<bool>),
    /// polyhedron(points, facens, convexity)
    ///
    /// TODO: convexity parameter
    /// Source: <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#polyhedron>
    Polyhedron(&'a [[f64; 3]], &'a [&'a [u32]]),
}

impl<'a> Scad for ThreeD<'a> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use ThreeD::*;

        match *self {
            Sphere(ref size) => match size {
                Some(CircleSize::Radius(radius)) => {
                    write!(f, "sphere(r={})", ToScad(radius))?;
                }
                Some(CircleSize::Diameter(diameter)) => {
                    write!(f, "sphere(d={})", ToScad(diameter))?;
                }
                None => {
                    write!(f, "sphere()")?;
                }
            },
            Cube(ref size, center) => {
                let center = center
                    .map(|center| {
                        if size.is_some() {
                            if center {
                                ",true"
                            } else {
                                ""
                            }
                        } else {
                            // specify parameter name if size is not given
                            if center {
                                "center=true"
                            } else {
                                ""
                            }
                        }
                    })
                    .unwrap_or("");
                match size {
                    Some(CubeSize::Scalar(side)) => write!(f, "cube({}{})", ToScad(side), center),
                    Some(CubeSize::Vector3(slice)) => {
                        write!(f, "cube({}{})", ToScad(slice), center)
                    }
                    None => write!(f, "cube({})", center),
                }?;
            }
            Polyhedron(ref points, ref faces) => {
                write!(
                    f,
                    "polyhedron(points={},faces={})",
                    ToScad(points),
                    ToScad(faces)
                )?;
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static STATIC_SPHERE: ThreeD = ThreeD::Sphere(Some(CircleSize::Diameter(8.5)));

    #[test]
    fn sphere() {
        assert_eq!(STATIC_SPHERE.to_scad(), "sphere(d=8.5)");

        let sphere = ThreeD::Sphere(Some(CircleSize::Radius(-0.0)));
        assert_eq!(sphere.to_scad(), "sphere(r=-0)");

        let sphere = ThreeD::Sphere(None);
        assert_eq!(sphere.to_scad(), "sphere()");
    }

    static STATIC_CUBE: ThreeD = ThreeD::Cube(Some(CubeSize::Vector3([1.0, 2.0, 3.0])), Some(true));

    #[test]
    fn cube() {
        assert_eq!(STATIC_CUBE.to_scad(), "cube([1,2,3],true)");

        let cube = ThreeD::Cube(None, None);
        assert_eq!(cube.to_scad(), "cube()");

        let cube = ThreeD::Cube(None, Some(false));
        assert_eq!(cube.to_scad(), "cube()");

        let cube = ThreeD::Cube(None, Some(true));
        assert_eq!(cube.to_scad(), "cube(center=true)");

        let cube = ThreeD::Cube(Some(CubeSize::Scalar(-0.0)), None);
        assert_eq!(cube.to_scad(), "cube(-0)");

        let cube = ThreeD::Cube(Some(CubeSize::Scalar(6.0)), Some(true));
        assert_eq!(cube.to_scad(), "cube(6,true)");

        let cube = ThreeD::Cube(Some(CubeSize::Vector3([1.0, 0.0, -0.0])), Some(false));
        assert_eq!(cube.to_scad(), "cube([1,0,-0])");
    }

    static STATIC_POLYHEDRON: ThreeD = ThreeD::Polyhedron(
        &[[1.0, -2.0, -0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]],
        &[&[0, 1, 2]],
    );

    #[test]
    fn polyhedron() {
        assert_eq!(
            STATIC_POLYHEDRON.to_scad(),
            "polyhedron(points=[[1,-2,-0],[1,1,0],[0,1,0]],faces=[[0,1,2]])"
        );
    }
}
