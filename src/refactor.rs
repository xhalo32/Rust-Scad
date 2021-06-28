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

impl<T: Scad> Scad for Option<T> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        if let Some(scad) = self {
            scad.write_scad(f);
        }
        Ok(())
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
/// this enum specifies which format to use. r=/d= is added by the caller
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

#[derive(Clone, Debug)]
pub enum CubeSize {
    Scalar(f64),
    Vector3([f64; 3]),
}

impl Scad for CubeSize {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use CubeSize::*;

        match self {
            Scalar(v) => v.write_scad(f),
            Vector3(v) => v.write_scad(f),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CylinderType {
    Cylinder(CircleSize),
    Cone(Option<CircleSize>, Option<CircleSize>),
}

impl Scad for CylinderType {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use CircleSize::*;
        use CylinderType::*;

        match self {
            Cylinder(Radius(size)) => write!(f, "r={}", size),
            Cylinder(Diameter(size)) => write!(f, "d={}", size),
            Cone(Some(Radius(size1)), None) => write!(f, "r1={}", size1),
            Cone(Some(Diameter(size1)), None) => write!(f, "d1={}", size1),
            Cone(None, Some(Radius(size2))) => write!(f, "r2={}", size2),
            Cone(None, Some(Diameter(size2))) => write!(f, "d2={}", size2),
            Cone(Some(Radius(size1)), Some(Radius(size2))) => {
                write!(f, "r1={},r2={}", size1, size2)
            }
            Cone(Some(Diameter(size1)), Some(Radius(size2))) => {
                write!(f, "d1={},r2={}", size1, size2)
            }
            Cone(Some(Radius(size1)), Some(Diameter(size2))) => {
                write!(f, "r1={},d2={}", size1, size2)
            }
            Cone(Some(Diameter(size1)), Some(Diameter(size2))) => {
                write!(f, "d1={},d2={}", size1, size2)
            }
            Cone(None, None) => Ok(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Center {
    True,
    False,
}

impl Scad for Center {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use Center::*;

        if let True = self {
            write!(f, "center=true")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum ThreeD<'a> {
    /// `sphere(radius | d=diameter)`
    ///
    /// Source: <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#sphere>
    Sphere(Option<CircleSize>),
    /// `cube(size, center)` or `cube([width,depth,height], center)`
    ///
    /// Source <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#cube>
    Cube(Option<CubeSize>, Option<Center>),
    /// `cylinder(h,r|d,center)` or `cylinder(h,r1|d1,r2|d2,center)`
    ///
    /// Source <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#cylinder>
    Cylinder(Option<f64>, Option<CylinderType>, Option<Center>),
    /// `polyhedron(points, facens, convexity)`
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
                let comma = if size.is_some() && matches!(center, Some(Center::True)) {
                    ","
                } else {
                    ""
                };
                let center = ToScad(&center);
                let size = ToScad(size);
                write!(f, "cube({}{}{})", size, comma, center)?;
            }
            Cylinder(height, ty, center) => match (height, ty, center) {
                (None, None, None) => {
                    write!(f, "cylinder()")?;
                }
                (Some(height), None, None) => {
                    write!(f, "cylinder({})", height)?;
                }
                (None, Some(ty), None) => {
                    write!(f, "cylinder({})", ToScad(&ty))?;
                }
                (None, None, Some(center)) => {
                    write!(f, "cylinder({})", ToScad(&center))?;
                }
                (Some(height), Some(ty), None) => {
                    write!(f, "cylinder({},{})", height, ToScad(&ty))?;
                }
                (None, Some(ty), Some(center)) => {
                    write!(f, "cylinder({},{})", ToScad(&ty), ToScad(&center))?;
                }
                (Some(height), None, Some(center)) => {
                    write!(f, "cylinder({},{})", height, ToScad(&center))?;
                }
                (Some(height), Some(ty), Some(center)) => {
                    write!(
                        f,
                        "cylinder({},{},{})",
                        height,
                        ToScad(&ty),
                        ToScad(&center)
                    )?;
                }
            },
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

/// Test that a given `scad` generates the given `string`
macro_rules! scad_test {
    ($scad:expr => $string:expr) => {
        assert!(
            $scad.to_scad() == $string,
            "Scad `{:?}` generates:\n          `{}`\nexpected: `{}`",
            $scad,
            $scad.to_scad(),
            $string
        )
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    static STATIC_SPHERE: ThreeD = ThreeD::Sphere(Some(CircleSize::Diameter(8.5)));

    #[test]
    fn sphere() {
        scad_test!(STATIC_SPHERE => "sphere(d=8.5)");
        scad_test!(
            ThreeD::Sphere(Some(CircleSize::Radius(-0.0))) =>
            "sphere(r=-0)"
        );
        scad_test!(ThreeD::Sphere(None) => "sphere()");
    }

    static STATIC_CUBE: ThreeD =
        ThreeD::Cube(Some(CubeSize::Vector3([1.0, 2.0, 3.0])), Some(Center::True));

    #[test]
    fn cube() {
        scad_test!(STATIC_CUBE => "cube([1,2,3],center=true)");
        scad_test!(ThreeD::Cube(None, None) => "cube()");
        scad_test!(ThreeD::Cube(None, Some(Center::False)) => "cube()");
        scad_test!(ThreeD::Cube(None, Some(Center::True)) => "cube(center=true)");
        scad_test!(ThreeD::Cube(Some(CubeSize::Scalar(-0.0)), None) => "cube(-0)");
        scad_test!(ThreeD::Cube(Some(CubeSize::Scalar(6.0)), Some(Center::True)) => "cube(6,center=true)");
        scad_test!(ThreeD::Cube(
            Some(CubeSize::Vector3([1.0, 0.0, -0.0])),
            Some(Center::False),
        ) => "cube([1,0,-0])");
    }

    static STATIC_CYLINDER: ThreeD = ThreeD::Cylinder(
        Some(5.5),
        Some(CylinderType::Cone(None, Some(CircleSize::Diameter(3.0)))),
        Some(Center::True),
    );

    #[test]
    fn cylinder() {
        scad_test!(STATIC_CYLINDER => "cylinder(5.5,d2=3,center=true)");
        scad_test!(ThreeD::Cylinder(None, None, None) => "cylinder()");
        scad_test!(ThreeD::Cylinder(Some(-0.0), None, None) => "cylinder(-0)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cylinder(CircleSize::Radius(2.0))),
            None,
        ) => "cylinder(r=2)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cylinder(CircleSize::Diameter(2.0))),
            None,
        ) => "cylinder(d=2)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(None, None)),
            None,
        ) => "cylinder()");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Radius(1.0)), None)),
            None,
        ) => "cylinder(r1=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Diameter(1.0)), None)),
            None,
        ) => "cylinder(d1=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(None, Some(CircleSize::Radius(1.0)))),
            None,
        ) => "cylinder(r2=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(None, Some(CircleSize::Diameter(1.0)))),
            None,
        ) => "cylinder(d2=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Radius(1.0)), Some(CircleSize::Radius(1.0)))),
            None,
        ) => "cylinder(r1=1,r2=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Diameter(1.0)), Some(CircleSize::Radius(1.0)))),
            None,
        ) => "cylinder(d1=1,r2=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Radius(1.0)), Some(CircleSize::Diameter(1.0)))),
            None,
        ) => "cylinder(r1=1,d2=1)");
        scad_test!(ThreeD::Cylinder(
            None,
            Some(CylinderType::Cone(Some(CircleSize::Diameter(1.0)), Some(CircleSize::Diameter(1.0)))),
            None,
        ) => "cylinder(d1=1,d2=1)");
        scad_test!(ThreeD::Cylinder(None, None, Some(Center::True)) => "cylinder(center=true)");
        scad_test!(ThreeD::Cylinder(
            Some(9.0),
            Some(CylinderType::Cone(Some(CircleSize::Radius(1.0)), Some(CircleSize::Diameter(1.0)))),
            Some(Center::True),
        ) => "cylinder(9,r1=1,d2=1,center=true)");
    }

    static STATIC_POLYHEDRON: ThreeD = ThreeD::Polyhedron(
        &[[1.0, -2.0, -0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]],
        &[&[0, 1, 2]],
    );

    #[test]
    fn polyhedron() {
        scad_test!(
            STATIC_POLYHEDRON =>
            "polyhedron(points=[[1,-2,-0],[1,1,0],[0,1,0]],faces=[[0,1,2]])"
        );
    }
}
