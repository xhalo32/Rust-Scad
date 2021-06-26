pub trait Scad {
    fn to_scad(&self) -> String;
}

pub struct Vector3([f32; 3]);

impl std::ops::Deref for Vector3 {
    type Target = [f32; 3];

    fn deref(&self) -> &[f32; 3] {
        &self.0
    }
}

impl Scad for Vector3 {
    fn to_scad(&self) -> String {
        self.as_ref().to_scad()
    }
}

pub enum ThreeD<'a> {
    Polyhedron(&'a [Vector3], &'a [&'a [u32]]),
}

impl<'a> Scad for ThreeD<'a> {
    fn to_scad(&self) -> String {
        use ThreeD::*;

        match self {
            Polyhedron(points, faces) => String::from(format!(
                "polyhedron(points={},faces={})",
                points.to_scad(),
                faces.to_scad()
            )),
        }
    }
}

#[duplicate::duplicate(primitive; [f32]; [u32])]
impl Scad for primitive {
    fn to_scad(&self) -> String {
        self.to_string()
    }
}

impl<T: Scad> Scad for &[T] {
    fn to_scad(&self) -> String {
        String::from(&format!(
            "[{}]",
            self.iter().map(Scad::to_scad).collect::<Vec<_>>().join(",")
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn polyhedron() {
        let polyhedron = ThreeD::Polyhedron(
            &[
                Vector3([0.0, 0.0, 0.0]),
                Vector3([1.0, 1.0, 1.0]),
                Vector3([0.0, 0.0, 1.0]),
            ],
            &[&[0, 1, 2]],
        );

        assert_eq!(
            polyhedron.to_scad(),
            "polyhedron(points=[[0,0,0],[1,1,1],[0,0,1]],faces=[[0,1,2]])"
        );
    }
}
