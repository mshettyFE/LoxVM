use crate::value::Value;
// really basic stack
pub struct LoxStack {
    stk: Vec<Value>,
}

impl LoxStack {
    pub fn new() -> Self{
        return LoxStack{stk: Vec::new()}
    }
    
    pub fn reset(&mut self) {
        self.stk.clear();
    }

    pub fn push(&mut self, val: Value){
        self.stk.push(val);
    }

    pub fn pop(&mut self) -> Option<Value> {
        return self.stk.pop();
    }

    pub fn peek(&self, distance: usize) -> Option<Value>{
        // distance denotes how far from the top of the stack you want to peek
        // a distance of 0 means you want the top

        // Need to subtract 1 to account for 0-based indexing
        let index = self.stk.len()-1-distance;
        self.stk.get(index).cloned()
    }

    pub fn print(&self){
        // convenience function to view the guts of the stack
        print!("        ");
        for item in &self.stk{
            print!("[ ");
            item.print_value();
            print!(" ]");
        }
        println!("");
    }
}
