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
    Cone(CircleSize, CircleSize),
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
    Cylinder(Option<f64>, Option<CylinderType>, Option<bool>),
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
                let s = ToScad(size);
                write!(f, "cube({}{}{})", s, comma, center)?;
            }
            Cylinder(height, ty, center) => {
                let comma = if center.is_some() { "," } else { "" };
                let center = ToScad(&center);
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

    static STATIC_CUBE: ThreeD =
        ThreeD::Cube(Some(CubeSize::Vector3([1.0, 2.0, 3.0])), Some(Center::True));

    #[test]
    fn cube() {
        assert_eq!(STATIC_CUBE.to_scad(), "cube([1,2,3],center=true)");

        let cube = ThreeD::Cube(None, None);
        assert_eq!(cube.to_scad(), "cube()");

        let cube = ThreeD::Cube(None, Some(Center::False));
        assert_eq!(cube.to_scad(), "cube()");

        let cube = ThreeD::Cube(None, Some(Center::True));
        assert_eq!(cube.to_scad(), "cube(center=true)");

        let cube = ThreeD::Cube(Some(CubeSize::Scalar(-0.0)), None);
        assert_eq!(cube.to_scad(), "cube(-0)");

        let cube = ThreeD::Cube(Some(CubeSize::Scalar(6.0)), Some(Center::True));
        assert_eq!(cube.to_scad(), "cube(6,center=true)");

        let cube = ThreeD::Cube(
            Some(CubeSize::Vector3([1.0, 0.0, -0.0])),
            Some(Center::False),
        );
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
