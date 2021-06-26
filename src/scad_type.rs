use nalgebra as na;
use std::string::String;
use std::vec::Vec;

///Trait for converting from rust types to strings compatible with openscad
pub trait ScadType {
    fn get_code(&self) -> String;
}

impl ScadType for na::Vector3<f32> {
    fn get_code(&self) -> String {
        String::from("[")
            + &self.x.get_code()
            + ","
            + &self.y.get_code()
            + ","
            + &self.z.get_code()
            + "]"
    }
}
impl ScadType for na::Vector2<f32> {
    fn get_code(&self) -> String {
        String::from("[") + &self.x.get_code() + "," + &self.y.get_code() + "]"
    }
}

use duplicate::duplicate;

#[duplicate(primitive; [bool]; [usize]; [f32]; [f64]; [u32]; [u64]; [i32]; [i64])]
impl ScadType for primitive {
    fn get_code(&self) -> String {
        self.to_string()
    }
}

impl<T: ScadType> ScadType for Vec<T> {
    fn get_code(&self) -> String {
        (&self[..]).get_code()
    }
}

impl<T: ScadType> ScadType for &[T] {
    fn get_code(&self) -> String {
        let mut result = "[".to_string();

        for elem in *self {
            result = result + &elem.get_code() + ",";
        }

        result += "]";

        result
    }
}

impl ScadType for String {
    fn get_code(&self) -> String {
        String::from("\"") + &self.clone() + "\""
    }
}

#[cfg(test)]
mod type_tests {
    use crate::scad_type::*;

    #[test]
    fn type_test() {
        //No more tests needed for now. I assume the to_string() function works
        //as expected
        assert_eq!(na::Vector3::new(0.0, 0.0, 0.0).get_code(), "[0,0,0]");
        assert_eq!(na::Vector3::new(-5.0, 0.0, 0.0).get_code(), "[-5,0,0]");
        assert_eq!(na::Vector3::new(1.0, 2.0, 3.0).get_code(), "[1,2,3]");

        assert_eq!(na::Vector2::new(1.0, 3.3).get_code(), "[1,3.3]");

        assert_eq!(vec!(1, 2, 3, 4, 5, 6).get_code(), "[1,2,3,4,5,6,]");
    }
}
