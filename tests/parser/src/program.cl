(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Main{
    io: IO <- new IO;

    main(): Int{
        (new Reader).run(io)
    };
};

class Reader{
    list: List <- new List;

    run(io: IO): Int{
        let flag: Bool <- true in {
            while flag loop
                {
                    io.out_string(">");
                    let str: String <- io.in_string() in {
                        if str = "d" then
                            list.print(io)
                        else if str = "e" then
                            evaluate()
                        else if str = "x" then
                            flag <- false
                        else
                            list <- list.cons(str)
                        fi fi fi;
                    };
                }
            pool;
            0;
        }
    };

    evaluate(): Int {
        {
            if list.get_item() = "+" then
                let str2int: A2I <- new A2I,
                    a: Int <- str2int.a2i(list.get_next().get_item()),
                    b: Int <- str2int.a2i(list.get_next().get_next().get_item()) in {
                        list <- list.get_next().get_next().get_next().cons(str2int.i2a(a + b));
                }
            else if list.get_item() = "s" then
                let tmp: String <- list.get_next().get_item() in {
                    list.get_next().set_item(list.get_next().get_next().get_item());
                    list.get_next().get_next().set_item(tmp);
                    list <- list.get_next();
                }
            else 0
            fi fi;
            0;
        }
    };
};

class List{
    item: String;
    next: List;

    is_nil(): Bool { true };

    get_item(): String { item };

    get_next(): List { next };

    set_item(in_item: String): String { item <- in_item };
    
    init(str: String, rest: List): List{
        {
            item <- str;
            next <- rest;
            self;
        }
    };

    cons(str: String): List{
        (new Cons).init(str, self)
    };

    print(io: IO): Int{
        if not is_nil() then
            {
                io.out_string(item);
                io.out_string("\n");
                next.print(io);
            }
        else 
            0
        fi
    };
};

class Cons inherits List{
    is_nil(): Bool { false };
};