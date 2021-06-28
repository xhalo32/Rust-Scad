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

#[derive(Clone, Copy, Debug)]
pub enum CommaVariant {
    Before,
    After,
}

use CommaVariant::*;

pub struct InsertComma<'scad, T: Scad>(CommaVariant, &'scad T);

impl<'scad, T: Scad> Scad for InsertComma<'scad, T> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        match self.0 {
            Before => write!(f, ",{}", ToScad(self.1)),
            After => write!(f, "{},", ToScad(self.1)),
        }
    }
}

impl<'scad, T: Scad> fmt::Display for InsertComma<'scad, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.write_scad(f)
    }
}

pub struct NamedArgument<'name, 'scad, T: Scad>(&'name str, &'scad T);

impl<'name, 'scad, T: Scad> Scad for NamedArgument<'name, 'scad, T> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "{}={}", self.0, ToScad(self.1))
    }
}

impl<'name, 'scad, T: Scad> fmt::Display for NamedArgument<'name, 'scad, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.write_scad(f)
    }
}

pub trait Scad {
    fn write_scad(&self, f: &mut Formatter) -> Result;
    fn to_scad(&self) -> String {
        ToScad(self).to_string()
    }
}

#[duplicate::duplicate(primitive; [f64]; [u32])]
impl Scad for primitive {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

impl Scad for &str {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "\"{}\"", self)
    }
}

impl<T: Scad> Scad for Option<T> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        if let Some(scad) = self {
            scad.write_scad(f)?;
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

// TODO rename and document
#[derive(Clone, Copy, Debug)]
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
pub struct Center(bool);

impl Scad for Center {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "center={}", self.0)?;
        Ok(())
    }
}

/// ### Convexity
///
/// An Integer. The convexity parameter specifies the maximum
/// number of front sides (back sides) a ray intersecting the
/// object might penetrate. This parameter is needed only for
/// correctly displaying the object in OpenCSG preview mode
/// and has no effect on the polyhedron rendering. Optional.
#[derive(Clone, Copy, Debug)]
pub struct Convexity(u32);

impl Scad for Convexity {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "convexity={}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Layer<'a>(&'a str);

impl<'a> Scad for Layer<'a> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        write!(f, "layer={}", self.0)
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
    Polyhedron(&'a [[f64; 3]], &'a [&'a [u32]], Option<Convexity>),
    /// `import("….ext")`
    ///
    /// Source: <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Importing_Geometry#import>
    Import(&'a str, Option<Convexity>, Option<Layer<'a>>),
    /// `linear_extrude(height,center,convexity,twist,slices)`
    ///
    /// Source: <https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Using_the_2D_Subsystem#Linear_Extrude>
    LinearExtrude(
        Option<f64>,
        Option<Center>,
        Option<Convexity>,
        Option<f64>,      // twist
        Option<u32>,      // slices
        Option<CubeSize>, // scale
    ),
}

impl<'a> Scad for ThreeD<'a> {
    fn write_scad(&self, f: &mut Formatter) -> Result {
        use ThreeD::*;

        match *self {
            Sphere(size) => match size {
                Some(CircleSize::Radius(radius)) => {
                    write!(f, "sphere(r={})", ToScad(&radius))?;
                }
                Some(CircleSize::Diameter(diameter)) => {
                    write!(f, "sphere(d={})", ToScad(&diameter))?;
                }
                None => {
                    write!(f, "sphere()")?;
                }
            },
            Cube(size, center) => {
                let comma_center = center.as_ref().map(|center| InsertComma(Before, center));
                if size.is_some() {
                    write!(f, "cube({}{})", ToScad(&size), ToScad(&comma_center))?;
                } else {
                    write!(f, "cube({})", ToScad(&center))?;
                }
            }
            Cylinder(height, ty, center) => match (height, ty, center) {
                (None, None, center) => {
                    write!(f, "cylinder({})", ToScad(&center))?;
                }
                (height, None, center) => {
                    let center = center.as_ref().map(|center| InsertComma(Before, center));
                    write!(f, "cylinder({}{})", ToScad(&height), ToScad(&center))?;
                }
                (height, Some(ty), center) => {
                    // If Cylindertype is (None, None) it writes nothing and a comma would be unnecessary
                    let comma = if height.is_some() && !matches!(ty, CylinderType::Cone(None, None))
                    {
                        ","
                    } else {
                        ""
                    };
                    let ty = ToScad(&ty);
                    let center = center.as_ref().map(|center| InsertComma(Before, center));
                    write!(
                        f,
                        "cylinder({}{}{}{})",
                        ToScad(&height),
                        comma,
                        ty,
                        ToScad(&center)
                    )?;
                }
            },
            Polyhedron(ref points, ref faces, convexity) => {
                let convexity = convexity
                    .as_ref()
                    .map(|convexity| InsertComma(Before, convexity));
                write!(
                    f,
                    "polyhedron(points={},faces={}{})",
                    ToScad(points),
                    ToScad(faces),
                    ToScad(&convexity)
                )?;
            }
            Import(ref path, convexity, layer) => {
                let convexity = convexity
                    .as_ref()
                    .map(|convexity| InsertComma(Before, convexity));
                let layer = layer.as_ref().map(|layer| InsertComma(Before, layer));
                write!(
                    f,
                    "import({}{}{})",
                    ToScad(path),
                    ToScad(&convexity),
                    ToScad(&layer)
                )?;
            }
            LinearExtrude(height, center, convexity, twist, slices, scale) => {
                let height = height
                    .as_ref()
                    .map(|height| NamedArgument("height", height));
                let twist = twist.as_ref().map(|twist| NamedArgument("twist", twist));
                let slices = slices
                    .as_ref()
                    .map(|slices| NamedArgument("slices", slices));
                let scale = scale.as_ref().map(|scale| NamedArgument("scale", scale));

                write!(f, "linear_extrude(")?;
                if height.is_some() {
                    write!(f, "{},", ToScad(&height))?;
                };
                if center.is_some() {
                    write!(f, "{},", ToScad(&center))?;
                }
                if convexity.is_some() {
                    write!(f, "{},", ToScad(&convexity))?;
                };
                if twist.is_some() {
                    write!(f, "{},", ToScad(&twist))?;
                };
                if slices.is_some() {
                    write!(f, "{},", ToScad(&slices))?;
                };
                write!(f, "{}", ToScad(&scale))?;
            }
        };
        Ok(())
    }
}

/// Test that a given `scad` generates the given `string`
#[macro_export]
macro_rules! scad_test {
    ($scad:expr, $string:expr) => {
        assert!(
            $scad.to_scad() == $string,
            "Scad `{:?}`\ngenerates: `{}`\n expected: `{}`",
            $scad,
            $scad.to_scad(),
            $string
        )
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use CircleSize::*;

    static STATIC_SPHERE: ThreeD = ThreeD::Sphere(Some(Diameter(8.5)));

    #[test]
    fn sphere() {
        scad_test!(STATIC_SPHERE, "sphere(d=8.5)");
        scad_test!(ThreeD::Sphere(Some(Radius(-0.0))), "sphere(r=-0)");
        scad_test!(ThreeD::Sphere(None), "sphere()");
    }

    static STATIC_CUBE: ThreeD =
        ThreeD::Cube(Some(CubeSize::Vector3([1.0, 2.0, 3.0])), Some(Center(true)));

    #[test]
    fn cube() {
        scad_test!(STATIC_CUBE, "cube([1,2,3],center=true)");
        scad_test!(ThreeD::Cube(None, None), "cube()");
        scad_test!(
            ThreeD::Cube(None, Some(Center(false))),
            "cube(center=false)"
        );
        scad_test!(ThreeD::Cube(None, Some(Center(true))), "cube(center=true)");
        scad_test!(ThreeD::Cube(Some(CubeSize::Scalar(-0.0)), None), "cube(-0)");
        scad_test!(
            ThreeD::Cube(Some(CubeSize::Scalar(6.0)), Some(Center(true))),
            "cube(6,center=true)"
        );
        scad_test!(
            ThreeD::Cube(Some(CubeSize::Vector3([1.0, 2.0, -0.0])), None),
            "cube([1,2,-0])"
        );
        scad_test!(
            ThreeD::Cube(
                Some(CubeSize::Vector3([1.0, 0.0, -0.0])),
                Some(Center(false)),
            ),
            "cube([1,0,-0],center=false)"
        );
    }

    static STATIC_CYLINDER: ThreeD = ThreeD::Cylinder(
        Some(5.5),
        Some(CylinderType::Cone(None, Some(Diameter(3.0)))),
        Some(Center(true)),
    );

    #[test]
    fn cylinder() {
        scad_test!(STATIC_CYLINDER, "cylinder(5.5,d2=3,center=true)");
        scad_test!(ThreeD::Cylinder(None, None, None), "cylinder()");
        scad_test!(ThreeD::Cylinder(Some(-0.0), None, None), "cylinder(-0)");
        scad_test!(
            ThreeD::Cylinder(None, Some(CylinderType::Cylinder(Radius(2.0))), None,),
            "cylinder(r=2)"
        );
        scad_test!(
            ThreeD::Cylinder(None, Some(CylinderType::Cylinder(Diameter(2.0))), None,),
            "cylinder(d=2)"
        );
        scad_test!(
            ThreeD::Cylinder(None, Some(CylinderType::Cone(None, None)), None,),
            "cylinder()"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Radius(1.0)), None)),
                None,
            ),
            "cylinder(r1=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Diameter(1.0)), None)),
                None,
            ),
            "cylinder(d1=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(None, Some(Radius(1.0)))),
                None,
            ),
            "cylinder(r2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(None, Some(Diameter(1.0)))),
                None,
            ),
            "cylinder(d2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Radius(1.0)), Some(Radius(1.0)))),
                None,
            ),
            "cylinder(r1=1,r2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Diameter(1.0)), Some(Radius(1.0)))),
                None,
            ),
            "cylinder(d1=1,r2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Radius(1.0)), Some(Diameter(1.0)))),
                None,
            ),
            "cylinder(r1=1,d2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Diameter(1.0)), Some(Diameter(1.0)))),
                None,
            ),
            "cylinder(d1=1,d2=1)"
        );
        scad_test!(
            ThreeD::Cylinder(None, None, Some(Center(true))),
            "cylinder(center=true)"
        );
        scad_test!(
            ThreeD::Cylinder(Some(5.0), None, Some(Center(true))),
            "cylinder(5,center=true)"
        );
        scad_test!(
            ThreeD::Cylinder(
                Some(-4.0),
                Some(CylinderType::Cone(None, Some(Radius(2.5)))),
                None
            ),
            "cylinder(-4,r2=2.5)"
        );
        scad_test!(
            ThreeD::Cylinder(
                None,
                Some(CylinderType::Cone(Some(Radius(2.5)), None)),
                Some(Center(true))
            ),
            "cylinder(r1=2.5,center=true)"
        );
        scad_test!(
            ThreeD::Cylinder(
                Some(9.0),
                Some(CylinderType::Cone(Some(Radius(1.0)), Some(Diameter(1.0)))),
                Some(Center(true)),
            ),
            "cylinder(9,r1=1,d2=1,center=true)"
        );
        scad_test!(
            ThreeD::Cylinder(
                Some(15.0),
                Some(CylinderType::Cone(None, None)),
                Some(Center(false)),
            ),
            "cylinder(15,center=false)"
        );
    }

    static STATIC_POLYHEDRON: ThreeD = ThreeD::Polyhedron(
        &[[1.0, -2.0, -0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]],
        &[&[0, 1, 2]],
        Some(Convexity(10)),
    );

    #[test]
    fn polyhedron() {
        scad_test!(
            STATIC_POLYHEDRON,
            "polyhedron(points=[[1,-2,-0],[1,1,0],[0,1,0]],faces=[[0,1,2]],convexity=10)"
        );
    }

    static STATIC_IMPORT: ThreeD = ThreeD::Import("./test.scad", Some(Convexity(10)), None);

    #[test]
    fn import() {
        scad_test!(STATIC_IMPORT, "import(\"./test.scad\",convexity=10)");
    }
}
