#[derive(Clone)]
pub struct Value {
   pub val: f64,
}

impl Value {
    pub fn new(v: f64) ->Self{
        Value{val:v}
    }
    pub fn print_value(&self){
        print!("{:<.5}",self.val);
    }
}

pub struct ValueArray{
    values: Vec<Value>,
}

impl ValueArray{
    pub fn new() -> Self{
        ValueArray {values: Vec::new()}
    }

    pub fn write_value(&mut self, new_val: Value){
        self.values.push(new_val);
    }

    pub fn get_value(&self, index: usize) -> Option<&Value>{
        return self.values.get(index);
    }

    pub fn get_count(&self) ->usize{
        self.values.len()
    }
}
