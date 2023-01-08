use currying::curry;
use currying::capp;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;

#[curry(Rc, Orig("o_"))]
fn add(x : u32, y : u32, z : u32) -> u32 {
    x + y + z
}

#[derive(Clone)]
struct Hello { 
    hi : u32
}

impl Hello {
    //#[currying::curry]
    fn new(x : u32) -> Self {
        Hello {hi : x}
    }
    fn get(&self) -> u32 {
        self.hi
    }

    #[curry(Rc("b_"), Rc("", no_bundle))]
    fn add_in(&self, x : u32) -> u32 {
        x + self.hi
    }

    fn add_update(&mut self, x : u32) -> u32 {
        self.hi = self.hi + x;
        self.hi
    }

    fn c_add_update<'a>(&'a mut self) ->
        Rc<dyn FnMut(u32) -> u32 + 'a> {
            let clos = |x| {
                self.hi = self.hi + x;
                self.hi
            };

            Rc::new(clos)
        }

}

struct RcHello { 
    hi : Rc<Mutex<u32>>
}

impl Clone for RcHello {
    fn clone(&self) -> Self {
        Self {
            hi : self.hi.clone(),
        }
    }
}

impl RcHello {
    //#[currying::curry]
    fn new(x : u32) -> Self {
        RcHello {hi : Rc::new(Mutex::new(x))}
    }
    fn get(&self) -> u32 {
        let lock = self.hi.lock().unwrap();
        *lock
    }

    #[curry(Rc("b_"), Rc("", no_bundle))]
    fn add_in(&self, x : u32) -> u32 {
        let lock = self.hi.lock().unwrap();
        x + *lock
    }

    fn add_update(&mut self, x : u32) -> u32 {
        let mut lock = self.hi.lock().unwrap();

        *lock = *lock + x;
        *lock
    }

    fn c_add_update<'a>(&'a mut self) ->
        Box<dyn FnMut(u32) -> u32 + 'a> {
            let clos = |x| {
                let mut lock = self.hi.lock().unwrap();
                *lock = *lock + x;
                *lock
            };
            Box::new(clos)
        }

}



#[test]
fn works(){
    let r1 = o_add(1, 2, 3);
    let r2 = add(1)(2)(3);
    assert!(r1 == r2);
}

#[test]
fn bundle() {
    let hello = Hello::new(2);
    let r1 = hello.add_in()(1);
    let r2 = hello.b_add_in(1);
    assert_eq!(r1, r2);
}

#[test]
fn do_capp() {
    let r1 = capp!(add 1 2 3);
    assert_eq!(r1, 6);
}

#[test]
fn wat() {
    let mut hello = Hello::new(3);
    let mut hello2 = hello.clone();
    let clos = hello2.c_add_update();
    hello.add_update(3);
    println!("we got {}", hello.get());
}

#[test]
fn wat2() {
    let mut hello = RcHello::new(3);
    let mut hello2 = hello.clone();
    let mut clos = hello2.c_add_update();
    hello.add_update(3);
    println!("we got {}", hello.get());
    (*clos)(4);
    println!("we got {}", hello.get());
       
}