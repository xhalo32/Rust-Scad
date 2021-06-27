use std::fmt::Write;

pub type Result = std::fmt::Result;

pub trait Scad {
    fn to_scad(&self) -> String {
        let mut buf = String::new();
        self.write_scad(&mut buf).unwrap();
        buf
    }

    fn write_scad(&self, buf: &mut String) -> Result;
}

/// Since scad allows creation of circle like objects using either radius or diameter,
/// this enum specifies which format to use
#[derive(Clone, Copy, Debug)]
pub enum CircleSize {
    Radius(f64),
    Diameter(f64),
}

impl Scad for CircleSize {
    fn write_scad(&self, buf: &mut String) -> Result {
        use CircleSize::*;

        match self {
            Radius(v) | Diameter(v) => v.write_scad(buf),
        }
    }
}

/// Scad allows using one number in place of a vector of 3 floats
/// with cubes for example
#[derive(Clone, Debug)]
pub enum CubeSize {
    Scalar(f64),
}

pub enum ThreeD<'a> {
    /// sphere(radius | d=diameter)
    ///
    /// Source: https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#sphere
    Sphere(Option<CircleSize>),
    /// cube(size, center)
    Cube(CubeSize, bool),
    /// polyhedron(points, faces, convexity)
    ///
    /// TODO: convexity parameter
    /// Source: https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Primitive_Solids#polyhedron
    Polyhedron(&'a [[f64; 3]], &'a [&'a [u32]]),
}

impl<'a> Scad for ThreeD<'a> {
    fn write_scad(&self, buf: &mut String) -> Result {
        use ThreeD::*;

        match *self {
            Sphere(size) => match size {
                Some(CircleSize::Radius(radius)) => {
                    write!(buf, "sphere(r=")?;
                    radius.write_scad(buf)?;
                    write!(buf, ")")?;
                }
                Some(CircleSize::Diameter(diameter)) => {
                    write!(buf, "sphere(d=")?;
                    diameter.write_scad(buf)?;
                    write!(buf, ")")?;
                }
                None => {
                    write!(buf, "sphere()")?;
                }
            },
            Cube(ref size, centered) => match size {
                CubeSize::Scalar(side) => {
                    write!(buf, "cube(")?;
                    side.write_scad(buf)?;
                    if centered {
                        write!(buf, ",true")?;
                    };
                    write!(buf, ")")?;
                }
            },
            Polyhedron(points, faces) => {
                write!(buf, "polyhedron(points=")?;
                points.write_scad(buf)?;
                write!(buf, ",faces=")?;
                faces.write_scad(buf)?;
                write!(buf, ")")?;
            }
        };
        Ok(())
    }
}

#[duplicate::duplicate(primitive; [f64]; [u32])]
impl Scad for primitive {
    fn write_scad(&self, buf: &mut String) -> Result {
        write!(buf, "{}", self)
    }
}

fn write_scad_iter<'a, T: Scad + 'a>(
    mut iter: impl Iterator<Item = &'a T>,
    buf: &mut String,
) -> Result {
    write!(buf, "[")?;
    if let Some(elem) = iter.next() {
        elem.write_scad(buf)?;
    }
    for elem in iter {
        write!(buf, ",")?;
        elem.write_scad(buf)?;
    }
    write!(buf, "]")
}

impl<T: Scad> Scad for [T] {
    fn write_scad(&self, buf: &mut String) -> Result {
        write_scad_iter(self.iter(), buf)
    }
}

impl<T: Scad> Scad for &[T] {
    fn write_scad(&self, buf: &mut String) -> Result {
        write_scad_iter(self.iter(), buf)
    }
}

impl<T: Scad, const N: usize> Scad for [T; N] {
    fn write_scad(&self, buf: &mut String) -> Result {
        write_scad_iter(self.iter(), buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sphere() {
        let sphere = ThreeD::Sphere(Some(CircleSize::Radius(-0.0)));
        assert_eq!(sphere.to_scad(), "sphere(r=-0)");

        let sphere = ThreeD::Sphere(Some(CircleSize::Diameter(5.0)));
        assert_eq!(sphere.to_scad(), "sphere(d=5)");

        let sphere = ThreeD::Sphere(None);
        assert_eq!(sphere.to_scad(), "sphere()");
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
        // let polyhedron = ThreeD::Polyhedron(
        //     &[
        //         Vector3([0.0, 0.0, 0.0]),
        //         Vector3([1.0, 1.0, 1.0]),
        //         Vector3([0.0, 0.0, 1.0]),
        //     ],
        //     &[&[0, 1, 2]],
        // );
        // assert_eq!(
        //     polyhedron.to_scad(),
        //     "polyhedron(poinnts=[[0,0,0],[1,1,1],[0,0,1]],faces=[[0,1,2]])"
        // );
    }
}
